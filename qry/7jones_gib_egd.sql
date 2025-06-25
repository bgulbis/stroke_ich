WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		-- ENCOUNTER.PERSON_ID,
		pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS FACILITY,
		NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_10_CODE,
		NOMENCLATURE.SOURCE_STRING AS DIAGNOSIS
	FROM
		ENCOUNTER,
		NOMENCLATURE,
		-- PERSON,
		PROCEDURE
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'10/01/2020 00:00:00'}, User:0), 
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
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		-- AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		ENCOUNTER.ENCNTR_ID = PROCEDURE.ENCNTR_ID
		AND PROCEDURE.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '0DJ08ZZ|^0DJ|0W3P8ZZ|^0W3P') > 0 
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836522 -- ICD-10-PCS
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 761 -- Procedure
		-- AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		-- AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
)

SELECT * FROM PATIENTS