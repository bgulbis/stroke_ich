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
	pi_get_cv_display(ORDERS.CATALOG_CD) AS SCAN,
	pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago') AS ORDER_DATETIME
FROM
	ORDERS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = ORDERS.ENCNTR_ID
	AND PATIENTS.PERSON_ID = ORDERS.PERSON_ID
	AND ORDERS.CATALOG_CD IN (
		9849927, -- Abdomen CTA
		73286694, -- Abdomen/Pelvis CTA
		3111259425, -- Abdomen/Pelvis GI Bleed CTA
		50724490, -- Brain/Neck CTA
		2990236965, -- Brain/Neck STROKE CTA
		9849777, -- Chest CTA
		34175125, -- Chest Pulmonary Embolism CTA
		68077110, -- Chest/Abd/Pelvis CTA
		384309162, -- Chest/Abdomen CTA
		1108556, -- Abdomen w IV contrast CT
		1108566, -- Abdomen w/wo IV contrast CT
		16081130, -- Abdomen/Pelvis w IV contrast CT
		1108566, -- Abdomen w/wo IV contrast CT
		1112640, -- Chest w contrast CT
		1112692, -- Chest w/wo contrast CT
		103748247, -- Chest/Abdomen w IV contrast CT
		449998116, -- Chest/Abd w/wo IV contrast CT
		16081414, -- Chest/Abdomen/Pelvis w IV contrast CT
		339126536, -- Chest/Abd/Pelvis w/wo IV contrast CT
		3183373149, -- Trauma Abdomen/Pelvis w IV contrast CT
		3095476905, -- Trauma Chest w contrast CT
		3183349297, -- Trauma Neck CTA
		10031431, -- Abdomen w/wo contrast MRI
		16945645, -- Abdomen/Pelvis w contrast MRI
		16945765, -- Abdomen/Pelvis w/wo contrast MRI
		1112657, -- Chest w contrast MRI
		1112701 -- Chest w/wo contrast MRI
	)
	AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)




/* 	AND CLINICAL_EVENT.EVENT_CD IN (
		10161815, -- Abdomen w/wo contrast MRA
		10034173, -- Abdomen with and without contrast MRI
		156933954, -- Abdomen w/wo contrast MRV
		1140849, -- Abdomen w/wo contrast CT
		10161814, -- Abdomen w/o contr MRA
		10034175, -- Abdomen without contrast MRI
		156933953, -- Abdomen w/o contrast MRV
		1140850, -- Abdomen wo contrast CT
		-- Abd/Pelv CTA
		3111362511, -- Abdomen/Pelvis GI Bleed CTA
		3641943113, -- Abdomen/Pelvis post EVAR CTA
		16961958, -- Abdomen/Pelvis w contrast MRI
		16084193, -- Abdomen/Pelvis w contrast CT
		16961959, -- Abdomen/Pelvis w/wo contrast MRI
		-- Abd/Pelv w/wo CT
		16961960, -- Abdomen/Pelvis wo contrast MRI
		50728906, -- Abdomen/Pelvis wo contrast CT
		386209004, -- Abdomen and Chest CTA
		9851881, -- Chest CTA
		
	) */