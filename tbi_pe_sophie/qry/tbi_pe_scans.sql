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
	-- TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'YYYY-MM-DD"T"HH24:MI:SS') AS EVENT_DATETIME,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS EVENT_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS EVENT
FROM
	CLINICAL_EVENT,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
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
		-- 9851881, -- Chest CTA	
		50728911 -- CTA Head/Neck CT
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
