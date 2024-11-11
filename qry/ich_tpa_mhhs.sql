WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE,
		PERSON.NAME_FULL_FORMATTED AS NAME,
		pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS FACILITY,
		-- ENCOUNTER.ARRIVE_DT_TM,
		-- ENCOUNTER.REG_DT_TM,
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
			5020309, -- Memorial Hermann Katy Hospital
			5022359, -- Memorial Hermann The Woodlands Hospital
			5022353, -- Memorial Hermann Southwest Hospital
			5022335, -- Memorial Hermann Memorial City Hospital
			5022350, -- Memorial Hermann Southeast Hospital
			5022344, -- Memorial Hermann Greater Heights
			9329954, -- Memorial Hermann Northeast Hospital
			15256492, -- Memorial Hermann Pearland Hospital
			15890188, -- Memorial Hermann Cypress Hospital
			5020318 -- Memorial Hermann Sugar Land			
			-- 7755245, -- Memorial Hermann TIRR
			-- 14658300, -- Memorial Hermann Orthopedic and Spine Hospital
			-- 11101594 -- Memorial Hermann Katy Rehabilitation Hospital
		)
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2019 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				pi_time_zone(1, @Variable('BOUSER'))
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'10/01/2024 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				pi_time_zone(1, @Variable('BOUSER'))
			)
		AND ENCOUNTER.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
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
		)
		-- AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		-- AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I61') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		-- AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
)

SELECT DISTINCT
    PATIENTS.ENCNTR_ID,
	ENCNTR_ALIAS.ALIAS AS FIN,
	PATIENTS.AGE,
	PATIENTS.NAME,
    pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS MED_DATETIME,
    CLINICAL_EVENT.EVENT_ID,
    pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MED,
    CE_MED_RESULT.ADMIN_DOSAGE,
    pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSAGE_UNIT,
    CE_MED_RESULT.INFUSION_RATE,
    pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS INFUSION_UNIT,
    pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ADMIN_ROUTE,
    pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
    pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
    CE_MED_RESULT,
    CLINICAL_EVENT,
	ENCNTR_ALIAS,
    ENCNTR_LOC_HIST,
    PATIENTS
WHERE
    PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
    AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
    AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
    AND CLINICAL_EVENT.EVENT_CD = 37556077 -- alteplase
    AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
    AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
    AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
    AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
    AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
    AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
        SELECT MAX(ELH.TRANSACTION_DT_TM)
        FROM ENCNTR_LOC_HIST ELH
        WHERE
            CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
            AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
    )
    AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
	AND PATIENTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1