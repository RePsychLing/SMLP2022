
On this OSF page you will find data sheets, Rmarkdown scripts as well as the experiment set up that were used 
in the NAD Maulwurf task online study.

For questions please contact Mireia Marimon (marimon@uni-potsdam.de)

A brief summary of the files on this page:

Data files
1. incl_corr_SRT.csv contains data of the reaction time task but only of correct answers and participants 
included in the analysis.

2. incl_SC.csv contains data of the stem completion task of the participants included in the analysis.

3. incl_SRT.csv contains data of the reaction time task of included participants regardless whether the answer
was correct or incorrect.

4. maulwurf.csv contains the raw data of every participant in both tasks and regardless whether included in the
analysis or not.

5. maulwurf_incl.csv contains data of the participants included in the analysis of both tasks.

Legend of the relevant column names:

anon_ID: anonymised ID of each participant
exp_dur_min: duration of the complete experiment (both tasks) in minutes
Accuracy: correct (1) or incorrect (0) answer given by the participant
Sex: sex of the participant
age: age of the participant
Version: which of the four experiment versions was done by the participant
Task_Nr: task number 1 is reaction time task, task number 2 is stem completion task
Task_Name: name of the task
trial_Nr: trial number
Item: stimuli presented to participant in the trial
Key_Press: keyboard response of the participant (P or Q)
Response: was keyboard response correct (1) or incorrect (0)
rt: reaction time in milliseconds
Block_Type: type block (practice, learning, disruption, recovery)
Block_Nr: block number, numerical coding of the different blocks (1 to 6)
trial_Block: trial number inside each block (e.g., 1 to 48 for learning block, 1 to 24 for disruption block)
Familiarity: familiarity of the verb stem for the participant (familiar, novel)
Target: was target syllable present or not

6. segment_info.csv contains relevant information about the stimuli presented during the two tasks.
In the Stem Completion task the stimuli were shortened by removing the suffix.

Legend of the relevant column names:

old_name: name given in past experiments with longer stimuli (e.g., van der Kant et al., 2020, la sorella sta cantando)
new_name: name given in our experiment (Maulwurf task)
Item: label used for the item in the experiment and in analysed data frames.
seg_label: uttered syllables in the segment
seg_start: onset of the segment referring to the longer stimuli 
seg_end: offset of the segment referring to the longer stimuli 
seg_dur: duration of the segment
item_dur: duration of the whole item (from seg_start to seg_end)
suffix_onset: time when suffix starts counted from 0, segment duration of the first three elements (verb stem + pause + suffix) added up together


Experiment
In this folder the experiment itself is made available.

1. Maulwurf_Children_M.json is the experiment which was designed with the online plattform Labvanced.

2. Maulwurf_Task_show.mp4 demonstrates how the experiment is displayed to the child and how the experiment works.
It includes the audio explanation of the tasks with English subtitles.

3. Examples of the stimuli (all possible combinations, the number refers to the different verb stems): 
	2PA.wav: puo_are
	08SG.wav: sta_ando
	20PG.wav: puo_ando
	17SA.wav: sta_are

Main Analysis
This folder provides the R code for the preprocessing of the data and the analysis.

1. Maulwurf_Script.Rmd contains the data preprocessing an analysis done with R in a Rmarkdown file.

2. Maulwurf_Script.Rproj R project containing the analysis done with R.


