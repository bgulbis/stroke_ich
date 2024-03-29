WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
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
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'11/01/2021 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'11/01/2023 00:00:00'}, User:1), 
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
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^S0[1-9]') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
), METFORMIN_DOSES AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		'TRUE' AS METFORMIN_INPT
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37557098, -- glyBURIDE-metformin
			37557534, -- metFORmin
			117038745, -- glipiZIDE-metformin
			117038857, -- metFORmin-pioglitazone
			117038858, -- metFORmin-rosiglitazone
			222403184, -- metformin-sitagliptin
			300078771, -- metformin-repaglinide
			609787616, -- metformin-saxagliptin
			750915369, -- linagliptin-metFORMIN
			926562606, -- alogliptin-metFORMIN
			1282999007, -- canagliflozin-metFORMIN
			1417998086, -- dapagliflozin-metFORMIN
			1793096266, -- empagliflozin-metFORMIN
			2737463777, -- ertugliflozin-metformin
			3431888925 -- empagliflozin/linagliptin/metformin
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), METFORMIN_HOME AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		'TRUE' AS METFORMIN_HOME
	FROM
		ORDERS,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = ORDERS.ENCNTR_ID
		AND ORDERS.ORIG_ORD_AS_FLAG = 2 -- Recorded/Home Medication
		AND ORDERS.CATALOG_CD IN (
			9911374, -- metFORMIN
			99263467, -- glipiZIDE-metformin
			99269351, -- metFORMIN-rosiglitazone
			118570659, -- metFORMIN-pioglitazone
			119655577, -- glyBURIDE-metformin
			216042607, -- metFORMIN-sitagliptin
			298225474, -- metFORMIN-repaglinide
			609787609, -- metFORMIN-saxagliptin
			750915362, -- linagliptin-metFORMIN
			926562599, -- alogliptin-metFORMIN
			1282999000, -- canagliflozin-metFORMIN
			1417998079, -- dapagliflozin-metFORMIN
			1793096259, -- empagliflozin-metFORMIN
			2737463763, -- ertugliflozin-metformin
			3431888911 -- empagliflozin/linagliptin/metformin
		)
), GCS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		MIN(CLINICAL_EVENT.RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM, CLINICAL_EVENT.EVENT_ID) AS GCS
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD IN (
			159, -- NUM
			162 -- TXT
		)
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 134422765 -- Glasgow Coma Score
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
	GROUP BY
		PATIENTS.ENCNTR_ID
)

SELECT
	PATIENTS.ENCNTR_ID,
	METFORMIN_DOSES.METFORMIN_INPT,
	METFORMIN_HOME.METFORMIN_HOME,
	GCS.GCS
FROM
	GCS,
	METFORMIN_DOSES,
	METFORMIN_HOME,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = METFORMIN_DOSES.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = METFORMIN_HOME.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = GCS.ENCNTR_ID(+)
