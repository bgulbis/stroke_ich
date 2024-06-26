WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS FACILITY,
		ENCOUNTER.ARRIVE_DT_TM,
		ENCOUNTER.REG_DT_TM,
		-- ENCOUNTER.DISCH_DT_TM
		-- ENCOUNTER.ADMIT_SRC_CD,
		pi_get_cv_display(ENCOUNTER.ADMIT_SRC_CD) AS ADMIT_SRC
		-- pi_get_cv_display(ENCOUNTER.DISCH_DISPOSITION_CD) AS DISCH_DISPOSITION
	FROM
		DIAGNOSIS,
		ENCOUNTER,
		NOMENCLATURE,
		PERSON
	WHERE
		ENCOUNTER.ORGANIZATION_ID IN (
			1, -- Memorial Hermann Hospital
			5022359, -- Memorial Hermann The Woodlands Hospital
			5022353, -- Memorial Hermann Southwest Hospital
			5022335 -- Memorial Hermann Memorial City Hospital
		)
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/2022 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				pi_time_zone(1, @Variable('BOUSER'))
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'04/01/2024 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				pi_time_zone(1, @Variable('BOUSER'))
			)
		AND ENCOUNTER.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			9351014, -- MC Mem City
			9351114, -- SW Southwest
			9351127 -- TW The Woodland
		)
		-- AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I61') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
), FIRST_ADMIT AS (
	SELECT DISTINCT
		PATIENTS.PERSON_ID,
		MIN(PATIENTS.ENCNTR_ID) KEEP (DENSE_RANK FIRST ORDER BY PATIENTS.ARRIVE_DT_TM) AS ENCNTR_ID,
		MIN(PATIENTS.FACILITY) KEEP (DENSE_RANK FIRST ORDER BY PATIENTS.ARRIVE_DT_TM) AS FACILITY,
		MIN(PATIENTS.ARRIVE_DT_TM) AS ARRIVE_DT_TM,
		MIN(PATIENTS.REG_DT_TM) AS REG_DT_TM,
		MIN(PATIENTS.ADMIT_SRC) KEEP (DENSE_RANK FIRST ORDER BY PATIENTS.ARRIVE_DT_TM) AS ADMIT_SRC
	FROM
		PATIENTS
	GROUP BY 
		PATIENTS.PERSON_ID
), SBP AS (
    SELECT DISTINCT
        FIRST_ADMIT.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		CLINICAL_EVENT.EVENT_ID,
        TO_NUMBER(REGEXP_REPLACE(CLINICAL_EVENT.RESULT_VAL, '>|<')) AS RESULT_VAL
    FROM
        CLINICAL_EVENT,
        FIRST_ADMIT
    WHERE
        FIRST_ADMIT.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
        AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
        AND FIRST_ADMIT.PERSON_ID = CLINICAL_EVENT.PERSON_ID
        AND CLINICAL_EVENT.EVENT_CD IN (
            30098, -- Systolic Blood Pressure
            134401648 -- Arterial Systolic BP 1
        )
        -- AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN PATIENTS.ARRIVE_DT_TM AND PATIENTS.ARRIVE_DT_TM + 0.25
        AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), PREGNANT AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		-- CLINICAL_EVENT.RESULT_VAL AS PREGNANT
		'TRUE' AS PREGNANT
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			33325, -- S Preg
			34136 -- U Preg
		)
		AND CLINICAL_EVENT.RESULT_VAL <> 'Negative'
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), DIALYSIS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		'TRUE' AS DIALYSIS
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			333892069, -- Hemodialysis Output Vol
			333892090, -- Peritoneal Dialysis Output Vol
			-- 333892112, -- CRRT Output Vol
			699896173, -- Hemodialysis Output Volume
			699896249 -- Peritoneal Dialysis Output Volume
			-- 173565025 -- CRRT Actual Pt Fluid Removed Vol
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
), HOME_MEDS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		pi_get_cv_display(ORDERS.CATALOG_CD) AS HOME_MED,
		'TRUE' AS MED
	FROM
		ORDERS,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = ORDERS.ENCNTR_ID
		AND ORDERS.ORIG_ORD_AS_FLAG = 2 -- Recorded/Home Medication
		AND ORDERS.CATALOG_CD IN (
			9902731, -- warfarin
			9907573, -- enoxaparin
			642177882, -- rivaroxaban
			894197557, -- apixaban
			1466817855, -- edoxaban
			545371153 -- dabigatran
		)
), HOME_MEDS_PIVOT AS (
	SELECT * FROM HOME_MEDS
	PIVOT(
		MIN(MED) FOR HOME_MED IN (
			'warfarin' AS HOME_WARFARIN,
			'enoxaparin' AS HOME_ENOXAPARIN,
			'apixaban' AS HOME_APIXABAN,
			'rivaroxaban' AS HOME_RIVAROXABAN,
			'edoxaban' AS HOME_EDOXABAN,
			'dabigatran' AS HOME_DABIGATRAN
		)
	)
), ANTICOAG AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		CASE
			WHEN CLINICAL_EVENT.EVENT_CD IN (37556169, 37556170) THEN 'argatroban'
			ELSE pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) 
		END AS MEDICATION,
		'TRUE' AS MED
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
			37558355, -- warfarin
			535736194, -- dabigatran
			642177890, -- rivaroxaban
			894197564 -- apixaban
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CE_MED_RESULT.IV_EVENT_CD > 0
		AND (
			(CLINICAL_EVENT.EVENT_CD IN (37557146, 37556313, 37556169, 37556170) AND CE_MED_RESULT.IV_EVENT_CD > 0)
			OR (CLINICAL_EVENT.EVENT_CD = 37556844 AND CE_MED_RESULT.ADMIN_DOSAGE > 40)
			OR (CLINICAL_EVENT.EVENT_CD IN (37558355, 535736194, 642177890, 894197564, 1466817862))
		)
), ANTICOAG_PIVOT AS (
	SELECT * FROM ANTICOAG
	PIVOT(
		MIN(MED) FOR MEDICATION IN (
			'heparin' AS INPT_HEPARIN,
			'enoxaparin' AS INPT_ENOXAPARIN,
			'bivalirudin' AS INPT_BIVALIRUDIN,
			'argatroban' AS INPT_ARGATROBAN,
			'warfarin' AS INPT_WARFARIN,
			'dabigatran' AS INPT_DABIGATRAN,
			'rivaroxaban' AS INPT_RIVAROXABAN,
			'apixaban' AS INPT_APIXABAN
		)
	)
)

SELECT DISTINCT
	FIRST_ADMIT.PERSON_ID,
	FIRST_ADMIT.ENCNTR_ID,
	ENCNTR_ALIAS.ALIAS AS FIN,
	FIRST_ADMIT.FACILITY,
	pi_from_gmt(FIRST_ADMIT.ARRIVE_DT_TM, 'America/Chicago') AS ARRIVE_DATETIME,
	pi_from_gmt(FIRST_ADMIT.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
	CASE
		WHEN FIRST_ADMIT.FACILITY = 'HH HERMANN' THEN 'TMC'
		ELSE 'MHHS'
	END AS LOCATION,
	FIRST_ADMIT.ADMIT_SRC,
	PREGNANT.PREGNANT,
	DIALYSIS.DIALYSIS,
	HOME_MEDS_PIVOT.HOME_WARFARIN,
	HOME_MEDS_PIVOT.HOME_ENOXAPARIN,
	HOME_MEDS_PIVOT.HOME_APIXABAN,
	HOME_MEDS_PIVOT.HOME_RIVAROXABAN,
	HOME_MEDS_PIVOT.HOME_EDOXABAN,
	HOME_MEDS_PIVOT.HOME_DABIGATRAN,
	ANTICOAG_PIVOT.INPT_HEPARIN,
	ANTICOAG_PIVOT.INPT_ENOXAPARIN,
	ANTICOAG_PIVOT.INPT_BIVALIRUDIN,
	ANTICOAG_PIVOT.INPT_ARGATROBAN,
	ANTICOAG_PIVOT.INPT_WARFARIN,
	ANTICOAG_PIVOT.INPT_DABIGATRAN,
	ANTICOAG_PIVOT.INPT_RIVAROXABAN,
	ANTICOAG_PIVOT.INPT_APIXABAN	
FROM
	ANTICOAG_PIVOT,
	ENCNTR_ALIAS,
	FIRST_ADMIT,
	DIALYSIS,
	HOME_MEDS_PIVOT,
	PREGNANT,
	SBP
WHERE
	FIRST_ADMIT.ENCNTR_ID = SBP.ENCNTR_ID
	AND SBP.EVENT_END_DT_TM BETWEEN FIRST_ADMIT.ARRIVE_DT_TM AND FIRST_ADMIT.ARRIVE_DT_TM + 1/8
	AND SBP.RESULT_VAL >= 150
	AND FIRST_ADMIT.ENCNTR_ID = PREGNANT.ENCNTR_ID(+)
	AND FIRST_ADMIT.ENCNTR_ID = DIALYSIS.ENCNTR_ID(+)
	AND FIRST_ADMIT.ENCNTR_ID = HOME_MEDS_PIVOT.ENCNTR_ID(+)
	AND FIRST_ADMIT.ENCNTR_ID = ANTICOAG_PIVOT.ENCNTR_ID(+)
	AND FIRST_ADMIT.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1
