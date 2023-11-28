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
		1140930, -- Brain w contrast CT
		1140934, -- Brain w/wo contrast CT
		1140937, -- Brain wo contrast CT
		1140940, -- Brain wo contrast limitedCT
		394305403, -- Brain Perfusion CT,
		597724409, -- Brain-Outside Consult CT
		883192194, -- Brain Stroke wo contrast CT
		883192201, -- Brain wo contrast Low-Dose CT
		2192270462, -- Stroke Brain Mobile w/o Contrast CT
		2647892741, -- Brain Stealth w contrast CT
		2647892759, -- Brain Stealth wo contrast CT
		3095663471, -- Trauma Brain w contrast CT
		3155213735, -- Trauma Brain wo contrast CT
		3576552547, -- Brain VNC CT
		3576552561, -- Brain w IV w Metal Reduction CT
		3576552575, -- Brain wo IVw Metal Reduction CT
		9851882, -- Head CTA
		50728911, -- CTA Head/Neck CT
		68083825, -- CT Brain/Carotid w contrast CTA
		883192208, -- Brain/Neck Stroke perfusion CTA
		930840066, -- Brain CTV
		2990362441, -- Brain/Neck STROKE CTA
		2990362455, -- Brain/Neck STROKE CTP
		1140919, -- Brain Pituitary w contrast MRI
		1140920, -- Brain Pituitary w contrast limited MRI
		1140922, -- Brain Pituitary w/wo contrast MRI
		1140924, -- Brain Pituitary wo contrast MRI
		1140925, -- Brain Pituitary wo contrast limited MRI
		1140929, -- Brain w contrast + additional MRI
		1140931, -- Brain w contrast MRI
		1140932, -- Brain w contrast limited MRI
		1140933, -- Brain w/wo contrast + additional MRI
		1140935, -- Brain w/wo contrast MRI
		1140936, -- Brain wo contrast + additional MRI
		1140938, -- Brain wo contrast MRI
		1140939 -- Brain wo contrast limited MRI
	
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
