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
)

SELECT DISTINCT
	PATIENTS.ENCNTR_ID,
	PATIENTS.FIN,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS MED_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
	pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
	CE_MED_RESULT.INFUSION_RATE AS RATE,
	pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS RATE_UNIT,
	pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
	pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ADMIN_ROUTE
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
	AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		37557146, -- heparin
		37556844, -- enoxaparin		
		37558355, -- warfarin
		535736194, -- dabigatran
		642177890, -- rivaroxaban
		894197564, -- apixaban	
		37556189, -- aspirin
		37556579, -- clopidogrel
		405732890, -- prasugrel
		652582238 -- ticagrelor
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
