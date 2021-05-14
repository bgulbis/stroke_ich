WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS,
		ENCOUNTER.REG_DT_TM,
		ENCOUNTER.DISCH_DT_TM
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), REVISITS AS (
	SELECT DISTINCT
		-- PATIENTS.ENCNTR_ID,
		PATIENTS.ALIAS AS FIN,
		pi_from_gmt(PATIENTS.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
		ENCNTR_ALIAS.ALIAS AS REVISIT_FIN,
		pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS REVISIT_ADMIT_DATETIME,
		pi_get_cv_display(ENCOUNTER.ENCNTR_TYPE_CLASS_CD) AS REVISIT_ENCNTR_TYPE,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS LAB_DATETIME,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.RESULT_VAL
		-- pi_get_cv_display(CLINICAL_EVENT.RESULT_UNITS_CD) AS RESULT_UNITS
	FROM
		CLINICAL_EVENT,
		ENCNTR_ALIAS,
		ENCOUNTER,
		PATIENTS
	WHERE
		PATIENTS.PERSON_ID = ENCOUNTER.PERSON_ID
		AND ENCOUNTER.REG_DT_TM BETWEEN PATIENTS.DISCH_DT_TM AND PATIENTS.DISCH_DT_TM + 365.25
		AND ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCOUNTER.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND ENCOUNTER.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 31090 -- Creatinine Lvl
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
)

SELECT
	FIN,
	DISCH_DATETIME,
	MAX(REVISIT_FIN) KEEP (DENSE_RANK FIRST ORDER BY LAB_DATETIME, EVENT_ID) AS REVISIT_FIN,
	MIN(REVISIT_ADMIT_DATETIME) AS REVISIT_ADMIT_DATETIME,
	MAX(REVISIT_ENCNTR_TYPE) KEEP (DENSE_RANK FIRST ORDER BY LAB_DATETIME, EVENT_ID) AS REVISIT_ENCNTR_TYPE,
	MIN(LAB_DATETIME) AS LAB_DATETIME,
	MAX(RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY LAB_DATETIME, EVENT_ID) AS SCR_VALUE	
FROM 
	REVISITS
GROUP BY
	FIN,
	DISCH_DATETIME