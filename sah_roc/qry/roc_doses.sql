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
), DOSES AS (
	SELECT DISTINCT
		PATIENTS.FIN,
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID,
		CASE
			WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN ORDERS.ORDER_ID
			ELSE ORDERS.TEMPLATE_ORDER_ID
		END AS ORIG_ORDER_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS MED_DATETIME,
		CLINICAL_EVENT.EVENT_ID,
		CE_MED_RESULT.ADMIN_DOSAGE
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
		AND CLINICAL_EVENT.EVENT_CD = 37557998 -- rocuronium
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.ADMIN_DOSAGE > 0
		AND CE_MED_RESULT.IV_EVENT_CD = 0
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
), DOSE_WT AS (
	SELECT DISTINCT
		DOSES.*,
		ORDER_DETAIL.OE_FIELD_DISPLAY_VALUE AS ORDER_WEIGHT
	FROM
		DOSES,
		ORDER_DETAIL
	WHERE
		DOSES.ORIG_ORDER_ID = ORDER_DETAIL.ORDER_ID(+)
		AND ORDER_DETAIL.OE_FIELD_MEANING_ID(+) = 99 -- WEIGHT
), WEIGHTS AS (
	SELECT DISTINCT
		DOSE_WT.ENCNTR_ID,
		DOSE_WT.ORIG_ORDER_ID,
		DOSE_WT.EVENT_END_DT_TM,
		MIN(CLINICAL_EVENT.RESULT_VAL) KEEP (DENSE_RANK LAST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM, CLINICAL_EVENT.EVENT_ID) AS WEIGHT_KG
	FROM
		CLINICAL_EVENT,
		DOSE_WT
	WHERE
		DOSE_WT.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND DOSE_WT.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 30107 -- Weight
		AND CLINICAL_EVENT.EVENT_END_DT_TM < DOSE_WT.EVENT_END_DT_TM
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.RESULT_UNITS_CD = 170 -- kg
	GROUP BY
		DOSE_WT.ENCNTR_ID,
		DOSE_WT.ORIG_ORDER_ID,
		DOSE_WT.EVENT_END_DT_TM
)

SELECT
	DOSE_WT.FIN,
	DOSE_WT.ENCNTR_ID,
	DOSE_WT.MED_DATETIME,
	DOSE_WT.EVENT_ID,
	DOSE_WT.ADMIN_DOSAGE,
	DOSE_WT.ORDER_WEIGHT,
	WEIGHTS.WEIGHT_KG
FROM
	DOSE_WT,
	WEIGHTS
WHERE
	DOSE_WT.ENCNTR_ID = WEIGHTS.ENCNTR_ID(+)
	AND DOSE_WT.ORIG_ORDER_ID = WEIGHTS.ORIG_ORDER_ID(+)
	AND DOSE_WT.EVENT_END_DT_TM = WEIGHTS.EVENT_END_DT_TM(+)
	