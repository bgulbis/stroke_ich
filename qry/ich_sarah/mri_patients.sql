WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS FACILITY,
		NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_CODE,
		NOMENCLATURE.SOURCE_STRING
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
			16125535, -- Memorial Hermann Sugar Land Hospital
			16126001 -- Memorial Hermann Sugar Land Hospital
		)
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2017 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				pi_time_zone(1, 'America/Chicago')
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'08/01/2021 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				pi_time_zone(1, 'America/Chicago')
			)
		-- AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND ENCOUNTER.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3796, -- HC Childrens
			-- 3821, -- HH Clinics
			-- 3822, -- HH Trans Care
			-- 3823, -- HH Rehab
			-- 95507, -- HH Comp Rehab C
			9326931, -- KM Katy
			9327051, -- SG Sugar Land
			9351014, -- MC Mem City
			9351079, -- GH Greater Heights
			9351108, -- SE Southeast
			9351114, -- SW Southwest
			9351127, -- TW The Woodland
			-- 119535990, -- TR TIRR
			171202052, -- NE Northeast
			-- 239872367, -- Memorial Hermann First Colony Hospital
			1788535501, -- HY OSH
			1824255295, --- BL PEARLAND
			2185384654 -- CY CYPRESS
			-- 2801312741 -- MEMORIAL HERMANN TOMBALL HOSPITAL		
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
		AND TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - PERSON.BIRTH_DT_TM) / 365.25, 0) >= 18
), MRI_SCANS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			1140919, -- Brain Pituitary w contrast MRI
			1140920, -- Brain Pituitary w contrast limited MRI
			1140922, -- Brain Pituitary w/wo contrast MRI
			1140924, -- Brain Pituitary wo contrast MRI
			1140925, -- Brain Pituitary wo contrast limited MRI
			1140929, -- Brain w contrast + additional MRI
			1140931, -- Brain w contrast MRI
			1140932, -- Brain w contrast limited MRI
			1140933, -- Brain w/wo contrast + additional MRI
			1140935, -- Brain w/wo contrast MRI
			1140936, -- Brain wo contrast + additional MRI
			1140938, -- Brain wo contrast MRI
			1140939 -- Brain wo contrast limited MRI
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
)

SELECT
	FACILITY,
	ICD_CODE,
	SOURCE_STRING,
	COUNT(DISTINCT PATIENTS.ENCNTR_ID) AS NUM_PTS
FROM
	MRI_SCANS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = MRI_SCANS.ENCNTR_ID
GROUP BY
	FACILITY,
	ICD_CODE,
	SOURCE_STRING
