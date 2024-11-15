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
	ORDERS.ORDER_ID,
	pi_get_cv_display(ORDERS.CATALOG_CD) AS MEDICATION,
	ORDERS.ORDERED_AS_MNEMONIC,
	PI_THERA_CLASS_VIEW.DRUG_CAT
FROM
	ORDERS,
	PATIENTS,
	PI_THERA_CLASS_VIEW
WHERE
	PATIENTS.ENCNTR_ID = ORDERS.ENCNTR_ID
	AND ORDERS.ORIG_ORD_AS_FLAG = 2 -- Recorded/Home Medication
	AND ORDERS.CATALOG_CD = PI_THERA_CLASS_VIEW.DRUG_CAT_CD(+)
