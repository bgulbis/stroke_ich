WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
	FROM
		ENCOUNTER
	WHERE
	    ENCOUNTER.ENCNTR_ID IN @prompt('Enter value(s) for Encntr Id','A',,Multi,Free,Persistent,,User:0)
), WEIGHTS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		MIN(CLINICAL_EVENT.RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM, CLINICAL_EVENT.EVENT_ID) AS WEIGHT
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE	
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 30107 -- Weight
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
		AND CLINICAL_EVENT.RESULT_UNITS_CD = 170 -- kg
	GROUP BY
		PATIENTS.ENCNTR_ID
), HEIGHTS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		MAX(CLINICAL_EVENT.RESUlT_VAL) KEEP (DENSE_RANK LAST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM, CLINICAL_EVENT.EVENT_ID) AS HEIGHT
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 30066 -- Height
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.RESULT_UNITS_CD = 164 -- cm
	GROUP BY
		PATIENTS.ENCNTR_ID
), BMI AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		MAX(CLINICAL_EVENT.RESUlT_VAL) KEEP (DENSE_RANK LAST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM, CLINICAL_EVENT.EVENT_ID) AS BMI
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 119838802 -- Body Mass Index
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		-- AND CLINICAL_EVENT.RESULT_UNITS_CD = 164 -- cm
	GROUP BY
		PATIENTS.ENCNTR_ID
)

SELECT DISTINCT
	PATIENTS.ENCNTR_ID,
	ENCNTR_ALIAS.ALIAS AS FIN,
	TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE,
	pi_get_cv_display(PERSON.SEX_CD) AS SEX,
	pi_get_cv_display(PERSON.RACE_CD) AS RACE,
	pi_get_cv_display(PERSON.ETHNIC_GRP_CD) AS ETHNICITY,
	WEIGHTS.WEIGHT,
	HEIGHTS.HEIGHT,
	BMI.BMI,
	ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,
	pi_get_cv_display(ENCOUNTER.DISCH_DISPOSITION_CD) AS DISCH_DISPOSITION,
	pi_from_gmt(ENCOUNTER.ARRIVE_DT_TM, 'America/Chicago') AS ARRIVE_DATETIME,
	pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
	pi_from_gmt(ENCOUNTER.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME
FROM
	BMI,
	ENCNTR_ALIAS,
	ENCOUNTER,
	HEIGHTS,
	PATIENTS,
	PERSON,
	WEIGHTS
WHERE
	PATIENTS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
	AND PATIENTS.PERSON_ID = PERSON.PERSON_ID
	AND PATIENTS.ENCNTR_ID = WEIGHTS.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = HEIGHTS.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = BMI.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1
