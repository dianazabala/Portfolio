Estimating Likelihood of Hospital Readmission for Coronary Artery Bypass Graft Surgery Patients: 
Project report, R script, graphs, output

This exercise deals with simulated administrative healthcare claims found in the file USN_claims_test_data.csv

Tasks:
1. Subsetted the dataset to the population at risk, admissions for patients undergoing isolated
coronary artery bypass grafts (CABG), identified by procedure codes of 3610, 3611, 3612,
3613, 3614, 3615, 3616, 3617, or 3619.

2. Identified comorbidities present in the diagnosis codes using the Charlson index.

3. Identified whether each admission involved a readmission. A readmission here is defined as a
subsequent hospitalization for the same patientId within 30 days of the index admission.

4. Specified and ran a regression model that estimates the likelihood of readmission among
patients admitted for CABG surgery. Controls for age, systolic blood pressure, and
comorbidities present in the admission record.

5. Interpreted the model output and explained model choice over the alternatives. Discussed
the assumptions that must hold in order to obtain unbiased estimates.
