WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
	FROM
		ENCOUNTER
	WHERE
	    ENCOUNTER.ENCNTR_ID IN @prompt('Enter value(s) for Encntr Id','A',,Multi,Free,Persistent,,User:0)
)

SELECT DISTINCT
	PATIENTS.ENCNTR_ID,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS EVENT_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS EVENT,
	CLINICAL_EVENT.RESULT_VAL,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	CLINICAL_EVENT,
	ENCNTR_LOC_HIST,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
	AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		31090, -- Creatinine Lvl
		30699, -- BUN
		32170, -- Potassium Lvl
		134422765, -- Glasgow Coma Score
		30098, -- Systolic Blood Pressure
		30051, -- Diastolic Blood Pressure
		134401648, -- Arterial Systolic BP 1
		134401703 -- Arterial Diastolic BP 1		
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
			AND ELH.ACTIVE_IND = 1
	)
	AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
	AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
