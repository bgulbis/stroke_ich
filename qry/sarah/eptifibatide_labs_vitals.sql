WITH PATIENTS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID,
		pi_from_gmt(ENCOUNTER.BEG_EFFECTIVE_DT_TM, 'America/Chicago') AS BEGIN_DATETIME,
		pi_from_gmt(ENCOUNTER.ARRIVE_DT_TM, 'America/Chicago') AS ARRIVE_DATETIME,
		pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
		pi_from_gmt(ENCOUNTER.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
		ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,	
		TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - PERSON.BIRTH_DT_TM) / 365.25, 0) AS AGE,
		pi_get_cv_display(PERSON.SEX_CD) AS SEX
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		PERSON
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37556867 -- eptifibatide
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2018 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'7/01/2021 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'America/Chicago'
			)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CE_MED_RESULT.IV_EVENT_CD IN (
			688706, -- Begin Bag
			688709 -- Rate Change
		)
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
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD IN (
			283905037, -- HH 7J
			193270 -- HH STRK
		)
		AND CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND CLINICAL_EVENT.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - PERSON.BIRTH_DT_TM) / 365.25, 0) >= 18
), PREGNANT AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
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
), INCLUDED AS (
	SELECT
		ENCNTR_ALIAS.ALIAS AS FIN,
		PATIENTS.*
	FROM
		ENCNTR_ALIAS,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID NOT IN (SELECT ENCNTR_ID FROM PREGNANT)
		AND PATIENTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
)

SELECT DISTINCT
	INCLUDED.FIN,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS EVENT_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS EVENT,
	CLINICAL_EVENT.RESULT_VAL,
	pi_get_cv_display(CLINICAL_EVENT.RESULT_UNITS_CD) AS RESULT_UNITS,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	CLINICAL_EVENT,
	ENCNTR_LOC_HIST,
	INCLUDED
WHERE
	INCLUDED.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD IN (
		159, -- NUM
		162 -- TXT
	)
	AND INCLUDED.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		31854, -- Hgb
		30098, -- Systolic Blood Pressure
		134401648, -- Arterial Systolic BP 1	
		1640362022, -- Aspirin Effect Platelet
		1640434193 -- Plavix Effect Platelet
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
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