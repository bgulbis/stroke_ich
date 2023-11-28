WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS FACILITY,
		ENCOUNTER.ARRIVE_DT_TM
		-- ENCOUNTER.REG_DT_TM,
		-- ENCOUNTER.DISCH_DT_TM
		-- ENCOUNTER.ADMIT_SRC_CD,
		-- pi_get_cv_display(ENCOUNTER.ADMIT_SRC_CD) AS ADMIT_SRC,
		-- pi_get_cv_display(ENCOUNTER.DISCH_DISPOSITION_CD) AS DISCH_DISPOSITION
	FROM
		DIAGNOSIS,
		ENCOUNTER,
		NOMENCLATURE,
		PERSON
	WHERE
		ENCOUNTER.ORGANIZATION_ID IN (
			1, -- Memorial Hermann Hospital
			5020309, -- Memorial Hermann Katy Hospital
			5022359, -- Memorial Hermann The Woodlands Hospital
			5022353, -- Memorial Hermann Southwest Hospital
			5022335, -- Memorial Hermann Memorial City Hospital
			5022350, -- Memorial Hermann Southeast Hospital
			5022344, -- Memorial Hermann Greater Heights
			9329954, -- Memorial Hermann Northeast Hospital
			15256492, -- Memorial Hermann Pearland Hospital
			15890188, -- Memorial Hermann Cypress Hospital
			5020318, -- Memorial Hermann Sugar Land			
			7755245, -- Memorial Hermann TIRR
			14658300, -- Memorial Hermann Orthopedic and Spine Hospital
			11101594 -- Memorial Hermann Katy Rehabilitation Hospital
		)
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2018 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				pi_time_zone(1, @Variable('BOUSER'))
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'07/01/2023 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				pi_time_zone(1, @Variable('BOUSER'))
			)
		AND ENCOUNTER.LOC_FACILITY_CD IN (
				3310, -- HH HERMANN
				-- 3796, -- HC Childrens
				-- 3821, -- HH Clinics
				-- 3822, -- HH Trans Care
				-- 3823, -- HH Rehab
				-- 1099966301, -- HH Oncology TMC
				9326931, -- KM Katy
				9327051, -- SG Sugar Land
				9351014, -- MC Mem City
				9351079, -- GH Greater Heights
				9351108, -- SE Southeast
				9351114, -- SW Southwest
				9351127, -- TW The Woodland
				-- 119535990, -- TR TIRR
				171202052, -- NE Northeast
				-- 1788535501, -- HY OSH
				1824255295, --- BL PEARLAND
				2185384654 -- CY CYPRESS
				-- 379859013 -- KR Katy Rehab
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
		MIN(PATIENTS.ARRIVE_DT_TM) AS ARRIVE_DT_TM
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
), HIGH_SBP AS (
	SELECT DISTINCT
		FIRST_ADMIT.PERSON_ID,
		FIRST_ADMIT.ENCNTR_ID,
		FIRST_ADMIT.FACILITY,
		CASE
			WHEN FIRST_ADMIT.FACILITY = 'HH HERMANN' THEN 'TMC'
			ELSE 'MHHS'
		END AS LOCATION
	FROM
		FIRST_ADMIT,
		SBP
	WHERE
		FIRST_ADMIT.ENCNTR_ID = SBP.ENCNTR_ID
		AND SBP.EVENT_END_DT_TM BETWEEN FIRST_ADMIT.ARRIVE_DT_TM AND FIRST_ADMIT.ARRIVE_DT_TM + 1/8
		AND SBP.RESULT_VAL >= 220
)

SELECT
	LOCATION,
	COUNT(DISTINCT PERSON_ID) AS NUM_PTS
FROM
	HIGH_SBP
GROUP BY
	LOCATION