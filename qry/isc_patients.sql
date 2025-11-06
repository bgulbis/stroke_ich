SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_CODE,
	NOMENCLATURE.SOURCE_STRING
FROM
	DIAGNOSIS,
	ENCNTR_ALIAS,
	ENCOUNTER,
	NOMENCLATURE,
	PERSON
WHERE
	ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
	AND ENCOUNTER.REG_DT_TM BETWEEN 
		pi_to_gmt(
			TO_DATE(
				@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/2020 00:00:00'}, User:0), 
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			), 
			pi_time_zone(1, @Variable('BOUSER'))
		)
		AND pi_to_gmt(
			TO_DATE(
				@Prompt('Enter end date', 'D', , mono, free, persistent, {'07/01/2020 00:00:00'}, User:1), 
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			) - 1/86400, 
			pi_time_zone(1, @Variable('BOUSER'))
		)
	AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
	AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
	AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
	AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
	AND DIAGNOSIS.DIAG_PRIORITY = 1
	AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
	AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I63|^I61') > 0
	AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
	AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
	AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
	AND TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) >= 18
	AND ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR