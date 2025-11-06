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
	PATIENTS.*,
	pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago') AS ORDER_DATETIME,
	pi_get_cv_display(ORDERS.CATALOG_CD) AS SCAN
FROM
	ORDERS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = ORDERS.ENCNTR_ID
	AND PATIENTS.PERSON_ID = ORDERS.PERSON_ID
	AND ORDERS.CATALOG_CD IN (
		34175125, -- Chest Pulmonary Embolism CTA
		267279, -- Extremity upper venous Doppler US
		267289, -- Extremity lower venous Doppler US
		1114271, -- Ext Lower Venous Doppler Bilat US
		1114409, -- Ext Lower Venous Doppler Unilat US
		1114471, -- Ext Upper Venous Doppler Bilat US
		1114608, -- Ext Upper Venous Doppler Unilat US
		538744224 -- Ext Upper & Lower Venous Doppler Bil US
	)
	AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
