WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		-- ENCOUNTER.ARRIVE_DT_TM,
		-- pi_from_gmt(ENCOUNTER.ARRIVE_DT_TM, 'America/Chicago') AS ARRIVE_DATETIME,
		-- ENCOUNTER.REG_DT_TM,
		pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
		-- ENCOUNTER.DISCH_DT_TM,
		pi_from_gmt(ENCOUNTER.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
		-- pi_get_cv_display(ENCOUNTER.ADMIT_SRC_CD) AS ADMIT_SRC,
		TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE,
		pi_get_cv_display(PERSON.SEX_CD) AS SEX,
		MIN(NOMENCLATURE.SOURCE_IDENTIFIER) KEEP (DENSE_RANK FIRST ORDER BY DIAGNOSIS.DIAG_PRIORITY) OVER (PARTITION BY ENCOUNTER.ENCNTR_ID) AS TBI_CODE
		-- NOMENCLATURE.SOURCE_STRING AS DIAGNOSIS,
		-- DIAGNOSIS.DIAG_PRIORITY
		-- pi_get_cv_display(PERSON.RACE_CD) AS RACE,
		-- ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS
	FROM
		DIAGNOSIS,
		ENCOUNTER,
		NOMENCLATURE,
		PERSON
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'10/01/2015 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'03/01/2023 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'America/Chicago'
			)
		-- AND ENCOUNTER.DISCH_DT_TM < DATE '2022-09-01'
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND ENCOUNTER.ENCNTR_TYPE_CD IN (
			29532, -- Inpatient
			29540 -- Observation			
		)
		-- AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		-- AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		-- AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^S02.0|^S02.1|^S02.8|^S02.91|^S04.0|^S06|^S07.1') > 0
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^S0[1-9]') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
), ANTICOAG_DIAG AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		CASE
			WHEN REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I82.4|^I26|^I80') > 0 THEN 'VTE'
			WHEN REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I48') > 0 THEN 'Afib'
			WHEN REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^Z95.4|^Z95.2|^Z95.3') > 0 THEN 'Valve'
		END AS ANTICOAG_DIAG,
		'TRUE' AS VAL
	FROM
		DIAGNOSIS,
		NOMENCLATURE,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I82.4|^I26|^I80|^I48|^Z95.4|^Z95.2|^Z95.3') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
), DIAG_PIVOT AS (
	SELECT * FROM ANTICOAG_DIAG
	PIVOT(
		MIN(VAL) FOR ANTICOAG_DIAG IN (
			'VTE' AS VTE,
			'Afib' AS AFIB,
			'Valve' AS VALVE
		)
	)
), DOSES AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		CASE
			WHEN pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) IN ('argatroban', 'argatroban 100mg/ml INJ 2.5ml') THEN 'argatroban'
			ELSE pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) 
		END AS MEDICATION,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS MED_DATETIME
	FROM 
		CE_MED_RESULT,
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37557146, -- heparin
			37556844, -- enoxaparin		
			37556313, -- bivalirudin
			37556169, -- argatroban
			37556170, -- argatroban 100mg/ml INJ 2.5ml
			37556189, -- aspirin
			37556579 -- clopidogrel
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (
			(CLINICAL_EVENT.EVENT_CD IN (37557146, 37556313, 37556169, 37556170) AND CE_MED_RESULT.IV_EVENT_CD > 0)
			OR (CLINICAL_EVENT.EVENT_CD = 37556844 AND CE_MED_RESULT.ADMIN_DOSAGE > 40)
			OR (CLINICAL_EVENT.EVENT_CD IN (37556189, 37556579))
		)
), DOSES_FIRST AS (
	SELECT
		ENCNTR_ID,
		MEDICATION,
		MIN(MED_DATETIME) AS MED_DATETIME
	FROM
		DOSES
	GROUP BY
		ENCNTR_ID,
		MEDICATION
), DOSES_PIVOT AS (
	SELECT * FROM DOSES_FIRST
	PIVOT(
		MIN(MED_DATETIME) FOR MEDICATION IN (
			'heparin' AS HEPARIN,
			'enoxaparin' AS ENOXAPARIN,
			'bivalirudin' AS BIVALIRUDIN,
			'argatroban' AS ARGATROBAN,
			'aspirin' AS ASPIRIN,
			'clopidogrel' AS CLOPIDOGREL
		)
	)
)

SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	PATIENTS.*,
	DIAG_PIVOT.VTE,
	DIAG_PIVOT.AFIB,
	DIAG_PIVOT.VALVE,
	DOSES_PIVOT.HEPARIN,
	DOSES_PIVOT.ENOXAPARIN,
	DOSES_PIVOT.BIVALIRUDIN,
	DOSES_PIVOT.ARGATROBAN,
	DOSES_PIVOT.ASPIRIN,
	DOSES_PIVOT.CLOPIDOGREL
FROM
	DIAG_PIVOT,
	DOSES_PIVOT,
	ENCNTR_ALIAS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = DIAG_PIVOT.ENCNTR_ID
	AND PATIENTS.ENCNTR_ID = DOSES_PIVOT.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1