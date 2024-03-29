WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS AS FIN
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), DIALYSIS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		PATIENTS.FIN,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS EVENT_DATETIME,
		CLINICAL_EVENT.EVENT_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS EVENT
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		--AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND CLINICAL_EVENT.EVENT_CD IN (
			333892069, -- Hemodialysis Output Vol
			333892090, -- Peritoneal Dialysis Output Vol
			333892112, -- CRRT Output Vol
			699896173, -- Hemodialysis Output Volume
			699896249, -- Peritoneal Dialysis Output Volume
			173565025 -- CRRT Actual Pt Fluid Removed Vol
		)
		-- AND CLINICAL_EVENT.EVENT_END_DT_TM >= pi_to_gmt(SYSDATE - 3, 'America/Chicago')
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
)

SELECT
	FIN,
	MIN(EVENT_DATETIME) AS EVENT_DATETIME,
	EVENT
FROM
	DIALYSIS
GROUP BY
	FIN,
	EVENT