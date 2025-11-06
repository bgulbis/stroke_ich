SELECT DISTINCT
	CV_CLIN_EV.CODE_VALUE,
	CV_CLIN_EV.DISPLAY
FROM
	CODE_VALUE,
	CODE_VALUE CV_CLIN_EV,
	PI_THERA_CLASS_VIEW
WHERE
	PI_THERA_CLASS_VIEW.DRUG_CAT IN (
		'ACE inhibitors with calcium channel blocking agents', 
		'ACE inhibitors with thiazides',
		'angiotensin converting enzyme (ACE) inhibitors',
		'angiotensin II inhibitors',
		'angiotensin II inhibitors with calcium channel blockers',
		'angiotensin II inhibitors with thiazides',
		'angiotensin receptor blockers and neprilysin inhibitors',
		'antiadrenergic agents, centrally acting',
		'antiadrenergic agents (central) with thiazides',
		'antiadrenergic agents, peripherally acting',
		'antiadrenergic agents (peripheral) with thiazides',
		'beta blockers, cardioselective',
		'beta blockers, non-cardioselective',
		'beta blockers with thiazides',
		'calcium channel blocking agents',
		'loop diuretics',
		'miscellaneous antihypertensive combinations',
		'miscellaneous diuretics',
		'potassium-sparing diuretics',
		'potassium sparing diuretics with thiazides',
		'thiazide and thiazide-like diuretics',
		'vasodilators'
	)
	AND PI_THERA_CLASS_VIEW.DRUG_CAT_CD = CODE_VALUE.CODE_VALUE
	AND CODE_VALUE.DISPLAY_KEY = CV_CLIN_EV.DISPLAY_KEY
	AND CV_CLIN_EV.CODE_SET = 72
	