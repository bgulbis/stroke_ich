WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS AS FIN,
		ENCOUNTER.DISCH_DT_TM
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
)

SELECT DISTINCT
	PATIENTS.FIN,
	ENCNTR_ALIAS.ALIAS AS RETURN_FIN,
	pi_get_cv_display(ENCOUNTER.LOC_FACILITY_CD) AS RETURN_FACILITY,
	pi_get_cv_display(ENCOUNTER.ENCNTR_TYPE_CLASS_CD) AS ENCNTR_TYPE_CLASS,
	TO_CHAR(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago'), 'YYYY-MM-DD"T"HH24:MI:SS') AS READMIT_DATETIME
FROM
	ENCNTR_ALIAS,
	ENCOUNTER,
	PATIENTS
WHERE
	PATIENTS.PERSON_ID = ENCOUNTER.PERSON_ID
	AND ENCOUNTER.REG_DT_TM > PATIENTS.DISCH_DT_TM
	AND ENCOUNTER.REG_DT_TM <= PATIENTS.DISCH_DT_TM + 365/2
	AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD IN (
			42631, -- Inpatient
			-- 55851, -- Emergency
			688523 -- Observation		
	) 
	AND ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1