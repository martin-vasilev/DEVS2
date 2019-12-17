cd('D:\R\DEVS2\');
addpath(genpath(cd));

[data, fs]= audioread('Experiment/sounds/standard.wav');
% downsample for nicer plotting:
[P,Q] = rat((fs/8)/fs);

data_new = resample(data,P,Q);

f1 = figure;
plot(data_new, 'LineWidth',1,'color',[115 187 113]/255);
ylim([-1.5 1.5]);
saveas(f1, 'std.eps');

%%
[data, fs]= audioread('Experiment/sounds/novel.wav');
% downsample for nicer plotting:
[P,Q] = rat((fs/8)/fs);

data_new = resample(data,P,Q);

f1 = figure;
plot(data_new, 'LineWidth',1,'color',[115 187 113]/255);
ylim([-1.5 1.5]);
saveas(f1, 'nov.eps');


% 
% dur= 40; % in ms
% Fs= 12000; % sampling Fq
% len= (dur/1000)*Fs;
% 
% f=400; % Hz
% Amp=1;
% ts=1/Fs;
% T=dur/1000;
% t=0:ts:T;
% y2= Amp* sin(2*pi*f*t);
% y2(2,:)= y2;
% y2= y2';
% data= y2;
% 
% data= cos2ramp(data, 5, Fs);
% 
% fade_samples = round(0.004.*Fs); % figure out how many samples fade is over
% fade_scale = linspace(0,1,fade_samples)'; % create fade
% 
% sig_faded = data;
% sig_faded(1:fade_samples) = data(1:fade_samples).*fade_scale; % apply fade
% 
% 
% f1 = figure;
% plot(data, 'LineWidth',2,'color',[115 187 113]/255);
% ylim([-1.5 1.5]);
% 
