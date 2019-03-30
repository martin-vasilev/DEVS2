function [divergence_strap_median] = survival_CI_DPA_0ms(straps) %enter the number of bootstraps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Appendix from Reingold & Sheridan (Quarterly Journal of Experimental Psychology). 
%
%Confidence Interval DPA procedure (CI-DPA)
%
%We are happy to answer any questions about the code (please e-mail hsheridan@albany.edu)
%If you are modifying the code for a new experiment, begin by changing the number of subjects, input file path, and input file name.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%The constants below can be changed as neccesary for each experiment.
subs = 64; %number of subjects in the experiment.
binsize = 1;  % survival bin size in ms
window = 800; % size of the window in ms. Window size can be increased to accomodate subjects/conditions with longer durations.
nbins = window/binsize; % survival bins % must divide evenly
b_starts = 0:binsize:binsize*nbins;


%criterion for divergence
DV_point_empirical_criterion = .015;
DV_runs_criterion = 10; 


%Define the columns in the data input file
subject = 1; %column 1 contains the subject number
duration = 2; %column 2 contains the fixation duration
condition = 3; %column 3 indicates the  condition (1 = fast condition, 2 = slow condition).


%input file path.
dat_dir = 'D:/R/DEVS2/DPA/'; % '/Users/lab/Dropbox/Appendix/'; %location of the data input spreadsheet


%Open the input file.
sprintf('%sf.txt',dat_dir) %display the name of the data input file to the user.
in = fopen(sprintf('%sDPA_120ms.txt',dat_dir),'rt');
z = fgetl(in);
dat = fscanf(in,'%f',[3,inf]);
dat=dat';
fclose(in);


%define matrices.
real_srvl = []; %real grand mean survival curves
srvl_straps = []; %matrix of strapped survival curves
grp_fixns= {}; %grp_fixns stores the duration data for each condition and subject
all_B = []; %the B condition (i.e., the slow condition)
all_A = []; %the A condition (i.e., the fast condition)
DV_strap_matrix = [];



    sub_ID = 0; %this variable store the participant ID

    
    for s = 1:subs; %for each subject, associate the x variable with the A condition data and the y variable with the B condition data (note: A is the fast condition, and B is the slow condition).

        sub_ID = sub_ID + 1; %increment the sub_ID by 1 (this way each subject has a unique ID, even if the same subjects were randomly selected multiple times in your simulation).

        x = dat(dat(:,subject)==s & dat(:,condition) == 1,duration);
        grp_fixns{sub_ID,1} = x; 
        all_A(length(all_A)+1:length(all_A)+length(x)) = x;

        y = dat(dat(:,subject)==s & dat(:,condition) == 2,duration);
        grp_fixns{sub_ID,2} = y;
        all_B(length(all_B)+1:length(all_B)+length(y)) = y;


        for b = 1:nbins %calculate the survival values for each of the time bins (these survival values will be used to create the survival figures).
           real_srvl(1,b,sub_ID) = length(find(x>b_starts(b)))/length(x);
           real_srvl(2,b,sub_ID) = length(find(y>b_starts(b)))/length(y);
           real_srvl(3,b,sub_ID) = real_srvl(2,b,sub_ID) - real_srvl(1,b,sub_ID);
        end   
    end    

    m_srvl = mean(real_srvl,3); %store the mean survival function (averaged across participants).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Generate bootstraps by randomly resampling (with replacement) the data
    % from each condition and for each participant.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
    %generate the bootstraps
    for bs = 1:straps
        fprintf(1,sprintf('%i..',bs)); %outprint the current bootstrap to the command window, so that the user can keep track of progress.
        if mod(bs,15)==1 & bs >1
            fprintf(1,'\n');
        end


        this_strap_dat = {};

        for sub_ID = 1:subs 
            for c = 1:2
                this_strap_dat{sub_ID,c} = randsample(grp_fixns{sub_ID,c},length(grp_fixns{sub_ID,c}),true); %randomly sample the data from each condition
                test =  this_strap_dat{sub_ID,c};
            end


            for b = 1:nbins
                this_strap_srvl(1,b,sub_ID) = length(find(this_strap_dat{sub_ID,1}>b_starts(b)))/length(this_strap_dat{sub_ID,1}); % compute survival percentages for each bin for this strap
                this_strap_srvl(2,b,sub_ID) = length(find(this_strap_dat{sub_ID,2}>b_starts(b)))/length(this_strap_dat{sub_ID,2});
                this_strap_srvl(3,b,sub_ID) = this_strap_srvl(2,b,sub_ID)-this_strap_srvl(1,b,sub_ID); %calculate the difference across conditions (condition B - condition A)        
            end           
        end

       srvl_straps(:,:,bs) = mean(this_strap_srvl,3);  %average across participants and save the strap
       
       

       %determine the divergence_point
       splitpoint = 0;    
       run_count = 0;
       
        for b = 1:nbins
            if srvl_straps(3,b,bs)>= DV_point_empirical_criterion;
                splitpoint = b;
                run_count = run_count+1;
                
            else
                run_count = 0; 
                
            end
            if run_count == DV_runs_criterion
               break
            end
        end
        divergence_point_strap = (splitpoint-(DV_runs_criterion-1));
        
        
       if  divergence_point_strap > 0; %if there is a divergence point...
           %calculate percentage below divergence
            percentage_below_strap = (100 - mean(srvl_straps(1:2, divergence_point_strap,bs))*100);



            DV_strap_matrix = [DV_strap_matrix; bs, divergence_point_strap, percentage_below_strap]; %save all straps in a matrix
       else
            %calculate percentage below divergence
            percentage_below_strap = -4;

            DV_strap_matrix = [DV_strap_matrix; bs, divergence_point_strap, percentage_below_strap]; %save all straps in a matrix
       end
       
       
       %save all straps in a text file
       textfilename_straps = 'Simulated_DPA_Output.txt';
       fid = fopen(textfilename_straps, 'wt');
       for i = 1:size(DV_strap_matrix,1);
           fprintf(fid, '%d\t%d\t%d\t\n', DV_strap_matrix(i,:));
       end
       fclose('all');
        
    end

    fprintf(1,'\n');


   %find median of divergence_points_strap
   divergence_strap_vector = DV_strap_matrix(:,2);
   divergence_strap_sorted = sort(divergence_strap_vector);
   
   divergence_strap_median = median(divergence_strap_sorted)
   divergence_strap_min = divergence_strap_sorted(1);
   divergence_strap_max = divergence_strap_sorted(end);
   
   divergence_strap_alpha = .025;

   divergence_strap_ci_upper = divergence_strap_sorted(floor(straps*(1- divergence_strap_alpha))) % upper bound on the difference
   divergence_strap_ci_lower = divergence_strap_sorted(ceil(straps* divergence_strap_alpha)) % lower bound on the difference
        

end



