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
		PATIENTS.ENCNTR_ID,
		PATIENTS.FIN,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS MED_DATETIME,
		CLINICAL_EVENT.EVENT_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
		pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
		CE_MED_RESULT.INFUSION_RATE AS RATE,
		pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS RATE_UNIT,
		pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
		pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ADMIN_ROUTE,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37556413, -- captopril
			37556838, -- enalapril
			37557440, -- lisinopril
			37556264, -- benazepril
			37557965, -- ramipril
			37557456, -- losartan
			37556407, -- candesartan
			37558314, -- valsartan
			37557327, -- irbesartan
			37557717, -- olmesartan
			37556113, -- amLODipine
			37557678, -- NIFEdipine
			37557675, -- niCARdipine
			359088148, -- clevidipine
			37556758, -- diltiazem
			37558329, -- verapamil
			37557376, -- labetalol
			37556889, -- esmolol
			37557572, -- metoprolol
			37556437, -- carvedilol
			37557927, -- propranolol
			37557177, -- hydrochlorothiazide
			37556524, -- chlorthalidone
			37556505, -- chlorothiazide
			37557570, -- metolazone
			37557039, -- furosemide
			37556347, -- bumetanide
			37558233, -- torsemide
			37556905, -- ethacrynic acid
			37558105, -- spironolactone
			56250547, -- eplerenone
			37558253, -- triamterene
			37556093, -- AMILoride
			37557172, -- hydrALAZINE
			37557688, -- nitroglycerin
			37557689, -- nitroprusside
			37557598, -- minoxidil
			37556577, -- clonidine
			37557129, -- guanfacine
			37556801, -- doxazosin
			37558147, -- tamsulosin	
			37558159, -- terazosin
			37557895, -- prazosin
			608248871, -- azilsartan
			1823215551, -- sacubitril-valsartan
			37557186, -- hydrochlorothiazide-spironolactone
			37557189, -- hydrochlorothiazide-triamterene
			37556200, -- atenolol
			37556202, -- atenolol-chlorthalidone
			48069137, -- bisoprolol
			37556311, -- bisoprolol-hydrochlorothiazide
			117038767, -- hydrochlorothiazide-metoprolol
			37557183, -- hydrochlorothiazide-propranolol
			37556265, -- benazepril-hydrochlorothiazide
			37556415, -- captopril-hydrochlorothiazide
			37556841, -- enalapril-hydrochlorothiazide
			37557180, -- hydrochlorothiazide-lisinopril
			37557181, -- hydrochlorothiazide-losartan
			37557190, -- hydrochlorothiazide-valsartan
			37556114, -- amLODipine-benazepril
			37557175, -- hydrALAZINE-hydrochlorothiazide
			37556525, -- chlorthalidone-clonidine
			9903263, -- vancomycin
			9902666, -- gentamicin
			9902998, -- tobramycin
			9903087, -- amikacin		
			37556129, -- amphotericin B
			37556131, -- amphotericin B lipid complex
			37556132, -- amphotericin B liposomal	
			37556039, -- acyclovir
			37557046, -- ganciclovir
			37557983, -- rifAMPin
			37557029, -- foscarnet
			65522979, -- adefovir
			117038635, -- cidofovir
			37557236, -- indinavir
			37557691, -- norepinephrine
			37556849, -- EPINephrine
			37558323, -- vasopressin
			37557816, -- phenylephrine
			63003651, -- DOBUTamine
			37557594, -- milrinone
			37556655, -- cycloSPORINE
			37558140, -- tacrolimus
			37556552, -- CISplatin
			37557263, -- interferon alfa-2a
			37557264, -- interferon alfa-2b
			37557549, -- methotrexate
			37557601, -- mitomycin
			37556434, -- carmustine
			37557653, -- naproxen
			37557218, -- ibuprofen
			37557372, -- ketorolac
			37556474, -- celecoxib
			37556189, -- aspirin
			37556735, -- diclofenac
			37556738, -- diclofenac-misoprostol
			37557240, -- indomethacin
			37557826, -- phenytoin
			37556066, -- allopurinol
			37557757, -- pamidronate
			37557397, -- lansoprazole
			37557721, -- omeprazole
			37557761, -- pantoprazole
			51208912 -- famotidine
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
)

SELECT
	FIN,
	MIN(MED_DATETIME) AS MED_DATETIME,
	MEDICATION
FROM
	DOSES
GROUP BY
	FIN,
	MEDICATION