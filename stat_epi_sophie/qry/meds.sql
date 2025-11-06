WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
	FROM
		ENCOUNTER
	WHERE
	    ENCOUNTER.ENCNTR_ID IN @prompt('Enter value(s) for Encounter ID','A',,Multi,Free,Persistent,,User:0)
)

SELECT DISTINCT
	PATIENTS.ENCNTR_ID,
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
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
	CASE WHEN ORDERS.PRN_IND = 1 THEN 'PRN' END AS PRN
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
		51208923, -- enalaprilat
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
		37557826, -- phenytoin
		37557411, -- levetiracetam
		37556785, -- divalproex sodium
		37558313, -- valproic acid
		37557455, -- LORAzepam
		37557589, -- midazolam
		37557925, -- propofol
		37557791, -- pentobarbital
		37557808, -- phenobarbital
		37556417, -- carbamazepine
		37556920, -- ethosuximide
		37556947, -- felbamate
		37557393, -- lamotrigine
		37558205, -- tiagabine
		37558230, -- topiramate
		37558387, -- zonisamide
		359088230, -- rufinamide
		363180766, -- lacosamide
		423546852, -- lacosamide
		801846223, -- ezogabine
		1049067006, -- perampanel
		2180033019, -- brivaracetam		
		743177897, -- clobazam
		37556575, -- clonazepam
		37556731, -- diazepam
		467366254, -- vigabatrin
		37557041, -- gabapentin
		87626018, -- pregabalin		
		37556849, -- EPINephrine
		37557691, -- norepinephrine
		37558389, -- DOPamine
		37557816, -- phenylephrine
		37558323, -- vasopressin
		63003651, -- DOBUTamine
		-- 37557342, -- isoproterenol
		37557594 -- milrinone				
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
