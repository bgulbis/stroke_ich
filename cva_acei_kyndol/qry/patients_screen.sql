WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		-- ENCOUNTER.ARRIVE_DT_TM,
		-- pi_from_gmt(ENCOUNTER.ARRIVE_DT_TM, 'America/Chicago') AS ARRIVE_DATETIME,
		ENCOUNTER.REG_DT_TM,
		pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago') AS ADMIT_DATETIME,
		ENCOUNTER.DISCH_DT_TM,
		pi_from_gmt(ENCOUNTER.DISCH_DT_TM, 'America/Chicago') AS DISCH_DATETIME,
		pi_get_cv_display(ENCOUNTER.ADMIT_SRC_CD) AS ADMIT_SRC,
		NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_10_CODE,
		NOMENCLATURE.SOURCE_STRING AS DIAGNOSIS
		-- TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) AS AGE,
		-- pi_get_cv_display(PERSON.SEX_CD) AS SEX,
		-- pi_get_cv_display(PERSON.RACE_CD) AS RACE,
		-- ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS
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
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'09/01/2017 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'09/01/2022 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'America/Chicago'
			)
		AND ENCOUNTER.DISCH_DT_TM < DATE '2022-09-01'
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND ENCOUNTER.ENCNTR_TYPE_CD IN (
			29532, -- Inpatient
			29540 -- Observation			
		)
		AND ENCOUNTER.ADMIT_SRC_CD = 9061 -- Emergency Room
		AND ENCOUNTER.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^I61|^I63') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
		AND ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND TRUNC((TRUNC(pi_from_gmt(ENCOUNTER.REG_DT_TM, 'America/Chicago')) - TRUNC(pi_from_gmt(PERSON.BIRTH_DT_TM, 'America/Chicago'))) / 365.25, 0) >= 18
), CKD_PTS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID
	FROM
		DIAGNOSIS,
		NOMENCLATURE,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
		AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
		-- AND DIAGNOSIS.DIAG_PRIORITY = 1
		AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^N18|^Z99.2|^Z94.0|^T86.1') > 0
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome
), FIRST_POT AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		MIN(CLINICAL_EVENT.RESULT_VAL) KEEP (DENSE_RANK FIRST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM, CLINICAL_EVENT.EVENT_ID) AS POTASSIUM
	FROM
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD = 32170 -- Potassium Lvl
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
	GROUP BY
		CLINICAL_EVENT.ENCNTR_ID
), HIGH_POT AS (
	SELECT DISTINCT ENCNTR_ID
	FROM FIRST_POT
	WHERE TO_NUMBER(REGEXP_REPLACE(POTASSIUM, '>|<')) >= 6
), ACEI_ALLERGY AS (
	SELECT DISTINCT
		-- PATIENTS.PERSON_ID,
		PATIENTS.ENCNTR_ID
		-- ALLERGY.ALLERGY_ID,
		-- NOMENCLATURE.SOURCE_STRING AS ALLERGY
	FROM
		ALLERGY,
		NOMENCLATURE,
		PATIENTS
	WHERE
		PATIENTS.PERSON_ID = ALLERGY.PERSON_ID
		AND ALLERGY.CREATED_DT_TM < PATIENTS.DISCH_DT_TM
		AND ALLERGY.END_EFFECTIVE_DT_TM >= PATIENTS.REG_DT_TM
		AND ALLERGY.SUBSTANCE_NOM_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND REGEXP_INSTR(LOWER(NOMENCLATURE.SOURCE_STRING), 'benazepril|captopril|enalapril|fosinopril|lisinopril|moexipril|perindopril|quinapril|ramipril|trandolapril|ace inhibitor|angiotensin converting enzyme') > 0
), ACEI_PATIENTS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		'ACEI' AS GRP
	FROM 
		CE_MED_RESULT,
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37556114, -- amLODipine-benazepril
			37556264, -- benazepril
			37556265, -- benazepril-hydrochlorothiazide
			37556284, -- bepridil
			37556413, -- captopril
			37556415, -- captopril-hydrochlorothiazide
			37556838, -- enalapril
			37556840, -- enalapril-felodipine
			37556841, -- enalapril-hydrochlorothiazide
			37557031, -- fosinopril
			37557032, -- fosinopril-hydrochlorothiazide
			37557180, -- hydrochlorothiazide-lisinopril
			37557184, -- hydrochlorothiazide-quinapril
			37557440, -- lisinopril
			37557610, -- moexipril
			37557796, -- perindopril
			37557954, -- quinapril
			37557965, -- ramipril
			37558240, -- trandolapril
			37558241, -- trandolapril-verapamil
			117038768, -- hydrochlorothiazide-moexipril
			1793096556 -- amLODIPine-perindopril
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'

)

SELECT DISTINCT
	PATIENTS.*,
	COALESCE(ACEI_PATIENTS.GRP, 'NO_ACEI') AS MED_GROUP
FROM 
	ACEI_PATIENTS,
	CE_MED_RESULT,
	CLINICAL_EVENT,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID NOT IN (SELECT ENCNTR_ID FROM CKD_PTS)
	AND PATIENTS.ENCNTR_ID NOT IN (SELECT ENCNTR_ID FROM HIGH_POT)
	AND PATIENTS.ENCNTR_ID NOT IN (SELECT ENCNTR_ID FROM ACEI_ALLERGY)
	AND PATIENTS.ENCNTR_ID = ACEI_PATIENTS.ENCNTR_ID(+)
	AND PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
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
		467366049, -- aliskiren-hydrochlorothiazide
		467366054, -- aliskiren-valsartan
		467366065, -- amlodipine-telmisartan
		538590863, -- aliskiren-amlodipine
		608248871, -- azilsartan
		608249443, -- aliskiren/amlodipine/hydrochlorothiazide
		750915783, -- azilsartan-chlorthalidone
		1049066672, -- riociguat
		1793096556, -- amLODIPine-perindopril
		1823215551, -- sacubitril-valsartan
		2664288743, -- nebivolol-valsartan
		2841974887, -- lofexidine
		3431887709, -- amlodipine-celecoxib
		3630724761, -- levamlodipine
		3674349325 -- vericiguat
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
