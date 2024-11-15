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
	CASE
		WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN ORDERS.ORDER_ID
		ELSE ORDERS.TEMPLATE_ORDER_ID
	END AS ORIG_ORDER_ID,
	-- TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'YYYY-MM-DD"T"HH24:MI:SS') AS MED_DATETIME,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS MED_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
	pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
	CE_MED_RESULT.INFUSION_RATE AS RATE,
	pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS RATE_UNIT,
	pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
	pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ADMIN_ROUTE,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCNTR_LOC_HIST,
	ORDERS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
	AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		37556077, -- alteplase
		37558157, -- tenecteplase
		1823215551, -- sacubitril-valsartan
		37556114, -- amLODipine-benazepril
		37556264, -- benazepril
		37556265, -- benazepril-hydrochlorothiazide
		37556407, -- candesartan
		37556408, -- candesartan-hydrochlorothiazide
		37556413, -- captopril
		37556415, -- captopril-hydrochlorothiazide
		37556838, -- enalapril
		37556840, -- enalapril-felodipine
		37556841, -- enalapril-hydrochlorothiazide
		37557031, -- fosinopril
		37557032, -- fosinopril-hydrochlorothiazide
		37557179, -- hydrochlorothiazide-irbesartan
		37557180, -- hydrochlorothiazide-lisinopril
		37557181, -- hydrochlorothiazide-losartan
		37557184, -- hydrochlorothiazide-quinapril
		37557187, -- hydrochlorothiazide-telmisartan
		37557190, -- hydrochlorothiazide-valsartan
		37557327, -- irbesartan
		37557440, -- lisinopril
		37557456, -- losartan
		37557610, -- moexipril
		37557717, -- olmesartan
		37557796, -- perindopril
		37557954, -- quinapril
		37557965, -- ramipril
		37558154, -- telmisartan
		37558240, -- trandolapril
		37558241, -- trandolapril-verapamil
		37558314, -- valsartan
		117038715, -- eprosartan-hydrochlorothiazide
		117038768, -- hydrochlorothiazide-moexipril
		117038770, -- hydrochlorothiazide-olmesartan
		259555334, -- amlodipine-olmesartan
		259555338, -- amlodipine-valsartan
		390605017, -- amlodipine/hydrochlorothiazide/valsartan
		467366065, -- amlodipine-telmisartan
		608248871, -- azilsartan
		750915783, -- azilsartan-chlorthalidone
		1793096556 -- amLODIPine-perindopril		
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
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
	AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
