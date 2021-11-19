WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCOUNTER.REG_DT_TM
	FROM
		DIAGNOSIS,
		ENCOUNTER,
		NOMENCLATURE,
		PERSON
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2017 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				pi_time_zone(1, @Variable('BOUSER'))
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'08/01/2021 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				pi_time_zone(1, @Variable('BOUSER'))
			)
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		-- AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I61') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
)

SELECT DISTINCT
	PATIENTS.ENCNTR_ID,
	ENCNTR_ALIAS.ALIAS AS FIN,
	-- CLINICAL_EVENT.EVENT_END_DT_TM,
	-- pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS EVENT_DATETIME,
	-- CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	-- CE_MED_RESULT.ADMIN_DOSAGE,
	-- pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSAGE_UNIT,
	-- CE_MED_RESULT.INFUSION_RATE,
	-- pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS INFUSION_UNIT,
	-- pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ADMIN_ROUTE,
	-- pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCNTR_ALIAS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
	AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		37556007, -- acebutolol
		37556076, -- alprostadil
		37556093, -- AMILoride
		37556094, -- AMILoride-hydrochlorothiazide
		37556113, -- amLODipine
		37556114, -- amLODipine-benazepril
		37556200, -- atenolol
		37556202, -- atenolol-chlorthalidone
		37556264, -- benazepril
		37556265, -- benazepril-hydrochlorothiazide
		37556266, -- bendroflumethiazide
		37556267, -- bendroflumethiazide-rauwolfia serpentina
		37556284, -- bepridil
		37556292, -- betaxolol
		37556311, -- bisoprolol-hydrochlorothiazide
		37556347, -- bumetanide
		37556407, -- candesartan
		37556408, -- candesartan-hydrochlorothiazide
		37556413, -- captopril
		37556415, -- captopril-hydrochlorothiazide
		37556437, -- carvedilol
		37556505, -- chlorothiazide
		37556507, -- chlorothiazide-methyldopa
		37556524, -- chlorthalidone
		37556525, -- chlorthalidone-clonidine
		37556577, -- clonidine
		37556693, -- deserpidine-methyclothiazide
		37556758, -- diltiazem
		37556801, -- doxazosin
		37556838, -- enalapril
		37556840, -- enalapril-felodipine
		37556841, -- enalapril-hydrochlorothiazide
		37556866, -- eprosartan
		37556889, -- esmolol
		37556905, -- ethacrynic acid
		37556949, -- felodipine
		37557031, -- fosinopril
		37557032, -- fosinopril-hydrochlorothiazide
		37557039, -- furosemide
		37557128, -- guanabenz
		37557129, -- guanfacine
		37557172, -- hydrALAZINE
		37557177, -- hydrochlorothiazide
		37557179, -- hydrochlorothiazide-irbesartan
		37557180, -- hydrochlorothiazide-lisinopril
		37557181, -- hydrochlorothiazide-losartan
		37557182, -- hydrochlorothiazide-methyldopa
		37557183, -- hydrochlorothiazide-propranolol
		37557184, -- hydrochlorothiazide-quinapril
		37557185, -- hydrochlorothiazide-reserpine
		37557186, -- hydrochlorothiazide-spironolactone
		37557187, -- hydrochlorothiazide-telmisartan
		37557188, -- hydrochlorothiazide-timolol
		37557189, -- hydrochlorothiazide-triamterene
		37557190, -- hydrochlorothiazide-valsartan
		37557202, -- hydroflumethiazide-reserpine
		37557234, -- indapamide
		37557327, -- irbesartan
		37557348, -- isradipine
		37557376, -- labetalol
		37557440, -- lisinopril
		37557456, -- losartan
		37557492, -- mannitol
		37557554, -- methyclothiazide
		37557558, -- methyldopa
		37557570, -- metolazone
		37557572, -- metoprolol
		37557598, -- minoxidil
		37557610, -- moexipril
		37557641, -- nadolol
		37557675, -- niCARdipine
		37557678, -- NIFEdipine
		37557681, -- nimodipine
		37557683, -- nisoldipine
		37557688, -- nitroglycerin
		37557689, -- nitroprusside
		37557717, -- olmesartan
		37557780, -- penbutolol
		37557796, -- perindopril
		37557841, -- pindolol
		37557859, -- polythiazide-reserpine
		37557895, -- prazosin
		37557927, -- propranolol
		37557954, -- quinapril
		37557965, -- ramipril
		37557977, -- reserpine
		37558102, -- sotalol
		37558105, -- spironolactone
		37558147, -- tamsulosin
		37558154, -- telmisartan
		37558159, -- terazosin
		37558209, -- timolol
		37558233, -- torsemide
		37558240, -- trandolapril
		37558241, -- trandolapril-verapamil
		37558253, -- triamterene
		37558257, -- trichlormethiazide
		37558314, -- valsartan
		37558329, -- verapamil
		48069128, -- alfuzosin
		48069137, -- bisoprolol
		56250547, -- eplerenone
		111416804, -- hydralazine-isosorbide dinitrate
		117038454, -- amLODipine-atorvastatin
		117038523, -- bendroflumethiazide-nadolol
		117038602, -- carteolol
		117038715, -- eprosartan-hydrochlorothiazide
		117038766, -- hydralazine-isosorbide dinitrate
		117038767, -- hydrochlorothiazide-metoprolol
		117038768, -- hydrochlorothiazide-moexipril
		117038770, -- hydrochlorothiazide-olmesartan
		117038850, -- mecamylamine
		117038886, -- nesiritide
		117038907, -- pamabrom
		117038947, -- polythiazide
		117038948, -- polythiazide-prazosin
		117038969, -- rauwolfia serpentina
		247281036, -- nebivolol
		259555334, -- amlodipine-olmesartan
		259555338, -- amlodipine-valsartan
		359088148, -- clevidipine
		390605017, -- amlodipine/hydrochlorothiazide/valsartan
		390605154, -- silodosin
		467366049, -- aliskiren-hydrochlorothiazide
		467366054, -- aliskiren-valsartan
		467366065, -- amlodipine-telmisartan
		535736254, -- dutasteride-tamsulosin
		538590863, -- aliskiren-amlodipine
		608248871, -- azilsartan
		608249443, -- aliskiren/amlodipine/hydrochlorothiazide
		750915783, -- azilsartan-chlorthalidone
		1793096556, -- amLODIPine-perindopril
		1823215551, -- sacubitril-valsartan
		2664288743, -- nebivolol-valsartan
		3431887709 -- amlodipine-celecoxib
	)
    AND CLINICAL_EVENT.EVENT_END_DT_TM < PATIENTS.REG_DT_TM + 1
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND PATIENTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ENCNTR_ALIAS.ACTIVE_IND = 1