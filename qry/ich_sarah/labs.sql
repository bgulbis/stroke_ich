WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS AS FIN,
		ENCOUNTER.REG_DT_TM
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), LABS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		PATIENTS.FIN,
		PATIENTS.REG_DT_TM,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		-- pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS EVENT_DATETIME,
		ABS(1 - (CLINICAL_EVENT.EVENT_END_DT_TM - PATIENTS.REG_DT_TM)) AS ADMIT_LAB_24HR_DIFF,
		CLINICAL_EVENT.EVENT_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS EVENT,
		CLINICAL_EVENT.RESULT_VAL,
		pi_get_cv_display(CLINICAL_EVENT.RESULT_UNITS_CD) AS RESULT_UNITS
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD IN (
			159, -- NUM
			162 -- TXT
		)
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			31821, -- HDL
			32227, -- LDL (Calculated)
			32228, -- LDL Direct
			134422765, -- Glasgow Coma Score
			134590635, -- NIH Stroke Score
			297370347, -- NIH Stroke Scale Assessment
			1466654557, -- MRS Modified Rankin Scales
			1466654584, -- MRS Total
			3398081391 -- Modified Rankin Scale Score
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), ADMIT_LABS AS (
	SELECT
		-- ENCNTR_ID,
		FIN,
		EVENT,
		MIN(RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY EVENT_END_DT_TM, EVENT_ID) AS LAB_RESULT,
		'ADMIT' AS LAB_TIMING
	FROM LABS
	GROUP BY
		ENCNTR_ID,
		FIN,
		EVENT
), DC_LABS AS (
	SELECT
		-- ENCNTR_ID,
		FIN,
		EVENT,
		MIN(RESULT_VAL) KEEP (DENSE_RANK LAST ORDER BY EVENT_END_DT_TM, EVENT_ID) AS LAB_RESULT,
		'DISCH' AS LAB_TIMING
	FROM LABS
	WHERE 
		EVENT IN (
			'Glasgow Coma Score',
			'MRS Modified Rankin Scales',
			'MRS Total',
			'Modified Rankin Scale Score'
		)
	GROUP BY
		ENCNTR_ID,
		FIN,
		EVENT
), DAY_ONE_LABS AS (
	SELECT
		-- ENCNTR_ID,
		FIN,
		EVENT,
		MIN(RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY ADMIT_LAB_24HR_DIFF) AS LAB_RESULT,
		'24_HR' AS LAB_TIMING
	FROM LABS
	WHERE 
		EVENT IN (
			'Glasgow Coma Score',
			'NIH Stroke Score',
			'NIH Stroke Scale Assessment'
		)
	GROUP BY
		ENCNTR_ID,
		FIN,
		EVENT
)

SELECT * FROM ADMIT_LABS

UNION

SELECT * FROM DC_LABS

UNION

SELECT * FROM DAY_ONE_LABS
