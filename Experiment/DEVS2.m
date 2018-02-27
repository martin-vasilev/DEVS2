% Distraction by deviant sounds during reading, Experiment 2
% 120 ms sounds with a sound timing delay (0 vs 120 ms)
% Using novel sounds on each deviant trial

% Martin R. Vasilev, 2018

global const; 

%% settings:
clear all;
clear mex;
clear functions;

cd('C:\Users\EyeTracker\Desktop\Martin Vasilev\DEVS2');
addpath([cd '\functions'], [cd '\corpus'], [cd '\design'], [cd '\sounds']);

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
importDesign;
load('sent.mat');
load('quest.mat');
load('hasQuest.mat');
load('corr_ans.mat');
const.ntrials= height(design);

whichDEV= find(strcmp('DEV', design.sound));
a= zeros(size(design,1),1);
for i=1:length(whichDEV)
    a(whichDEV(i))= i; 
end
design.flnm= a;

%% Run Experiment:
runTrials;

%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');