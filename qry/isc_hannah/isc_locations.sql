WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS,
		ENCOUNTER.DISCH_DT_TM
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), UNIT_IN_OUT AS (
	SELECT DISTINCT
		ENCNTR_LOC_HIST.ENCNTR_ID,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM,
		LEAST(ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM, PATIENTS.DISCH_DT_TM) - ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AS TRANSACTION_DURATION,
		CASE
			WHEN 
				LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN 1
			ELSE 0
		END AS UNIT_IN
	FROM
		ENCNTR_LOC_HIST,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND PATIENTS.DISCH_DT_TM >= ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM
), UNIT_COUNT AS (
	SELECT DISTINCT
		UNIT_IN_OUT.ENCNTR_ID,
		UNIT_IN_OUT.LOC_NURSE_UNIT_CD,
		SUM(UNIT_IN_OUT.UNIT_IN) OVER (PARTITION BY UNIT_IN_OUT.ENCNTR_ID ORDER BY UNIT_IN_OUT.BEG_EFFECTIVE_DT_TM) AS UNIT_COUNT,
		UNIT_IN_OUT.TRANSACTION_DURATION
	FROM
		UNIT_IN_OUT
), UNIT_LOS AS (
	SELECT
		UNIT_COUNT.ENCNTR_ID,
		UNIT_COUNT.UNIT_COUNT,
		UNIT_COUNT.LOC_NURSE_UNIT_CD,
		SUM(UNIT_COUNT.TRANSACTION_DURATION) AS UNIT_LOS
	FROM
		UNIT_COUNT
/* 	WHERE
		UNIT_COUNT.LOC_NURSE_UNIT_CD IN (
			4122, -- HH MICU
			4137, -- HH CCU
			5441, -- HH STIC
			5541, -- HH CVICU
			27226894, -- HH TSIC
			283905037, -- HH 7J
			1993318732 -- HH HFIC
		)
 */	GROUP BY
		UNIT_COUNT.ENCNTR_ID,
		UNIT_COUNT.UNIT_COUNT,
		UNIT_COUNT.LOC_NURSE_UNIT_CD
)

SELECT
	PATIENTS.ALIAS AS FIN,
	UNIT_LOS.UNIT_COUNT,
	pi_get_cv_display(UNIT_LOS.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
	UNIT_LOS.UNIT_LOS
FROM
	PATIENTS,
	UNIT_LOS
WHERE
	PATIENTS.ENCNTR_ID = UNIT_LOS.ENCNTR_ID