% --------+---------+---------------+-----------+----------+ %
% Author: Blake Conrad                                       %
% Contents: Data Science Capstone                            %
% Data: FBI Crime Data from 1980-2014                        %
% --------+---------+---------------+-----------+----------+ %

% ---------+----------------+--------------------+---------- %
% Structure of the Data Set.
% --------+---------+---------------+-----------+----------+ %
%                                                            %
% S =                                                        %
%     X: [302595x10 double] <-- [302595x10] 10 col           %
%     G: [302595x1 double]  <-- [302595x1] 1 col             %
%     A: {1x302595 cell}    <-- [1x302595] 1 row             %
%     S: {1x302595 cell}    <-- [1x302595] 1 row             %
%     P: [1x302595 double]  <-- [1x302595] 1 row             %
%                                                            %
% --------+---------+---------------+-----------+----------+ %

% --------+---------+---------------+-----------+----------+ %
% Structure of the Variables
% --------+---------+---------------+-----------+----------+ %
%                                                            %
% A (City)| G (Year)| P (Population)| S (States)| X (Crime)| %
%               ^                                   ^        %
%          1-35 -> 1980-2014                        ^        %
%                                                   ^        %
%                                                (cols for X)%
%                                                   ^        %
%   0         1           2           3           4          %
% murder|manslaughter|rapetotal|roberytotal|assaulttotal     %
%                                                            %
%   5               6           7           8                %
% burglarytotal|larcenytotal|autotheft|numberOfficersKilled  %
%                                                            %
%   9                                                        %
% numberOfficersAssaulted                                    %
%                                                            %
% --------+---------+---------------+-----------+----------+ %

% ---------+----------------+--------------------+---------- %
% MATLAB Initialization Commands
% ---------+----------------+--------------------+---------- %
clear variables;
close all;
clc;
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Load Data
% ---------+----------------+--------------------+---------- %
S = load('1980_2014_forBlake.mat');
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Reshape vectors to [302595x1] then concat them into a table
% ---------+----------------+--------------------+---------- %
city                    = reshape(S.A,[302595 1]);
state                   = reshape(S.S,[302595 1]);
year                    = S.G;
population              = reshape(S.P,[302595 1]);
murder                  = S.X(:,1);
manslaughter            = S.X(:,2);
rapetotal               = S.X(:,3);
roberytotal             = S.X(:,4);
assaulttotal            = S.X(:,5);
burglarytotal           = S.X(:,6);
larcenytotal            = S.X(:,7);
autotheft               = S.X(:,8);
numberOfficersKilled    = S.X(:,9);
numberOfficersAssaulted = S.X(:,10);
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% New Tabular Data Format
% +--------+----------------+--------------------+----------+%
% | city | state | year | population | .... crimeStats ..... |
% +--------+----------------+--------------------+----------+%
T = table(city,state,year,population,murder,manslaughter,rapetotal,roberytotal,assaulttotal,burglarytotal,larcenytotal,autotheft,numberOfficersKilled,numberOfficersAssaulted);
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Data Manipulation in a Table by Condition
% ---------+----------------+--------------------+---------- %
log_indices =  strfind(T.city,'INDIANAPOLIS');
act_indices = find(not(cellfun('isempty', log_indices)));
row_hits = strcmp(T.city(:),'INDIANAPOLIS') & strcmp(T.state(:), '13');
hits_table = T(row_hits,:);

% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Group Statistics
% ---------+----------------+--------------------+---------- %
% For Population Included
% ST = grpstats(T(:,4:14));

% For Just Crime Statistics
ST = grpstats(T(:,5:14));
% ST
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Constants
% ---------+----------------+--------------------+---------- %
colnames = T.Properties.VariableNames;
numrows = height(T);
numcols = width(T);
start_crime_stats_index = 5;
end_crime_stats_index = 14;
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Create a Plot for Frequency vs. Crime over All Variables
% % ---------+----------------+--------------------+---------- %
% figure();
% for i=start_crime_stats_index:end_crime_stats_index
%     subplot(3,4,i-3);
%     plot(T.(i));
%     name = T.Properties.VariableNames(i);
%     xlabel(name);
%     ylabel('frequency');
%     title(name);
% end
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Generic Plot for Each Observations Murder
% ---------+----------------+--------------------+---------- %
% figure;
% plt = plot(T.murder(1:1000)); hold on;
% plt.Color = 'blue';
% x = plt.XData;
% y = plt.YData;
% [u,v] = prepareCurveData(x,y);
% f = fit(u,v,'smoothingspline');
% title('Murders at each observation in the data set');
% plt_f = plot(f,u,v);
% plt_f(1).Color = 'black';
% plt_f(2).Color = 'red';
% xlabel('Number of murders');
% ylabel('freq of of a given number of murders'); hold off;
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Best Fit Curves for Crime in Indianapolis from 1980-2014
% Over Each Crime Statistic Variable
% ---------+----------------+--------------------+---------- %
% figure();
% for i = start_crime_stats_index:end_crime_stats_index
%     % Get Observations Containing Indianapolis
%     indy_rows = strcmp(T.city(:),'INDIANAPOLIS');
% 
%     % Get Crime Specifically for Indianapolis
%     arg1_crime = table2array(T(indy_rows,i));
% 
%     % Get Cooresponding Years that Matched the Crime
%     arg2_years = T(indy_rows,'year');
% 
%     % Convert the Table Object to a Matrix of Doubles
%     arg2_years = [arg2_years.(1)];
% 
%     % Begin Subploting in a 2x5 plot-space
%     subplot(2,5,i-4);
% 
%     % Instantiate a Bar Graph Object for Customization
%     plt = bar(arg2_years, arg1_crime); hold on;
% 
%     % Create a Best Fit Line (Curve)
%     x = plt.XData;
%     y = plt.YData;
%     [u,v] = prepareCurveData(x,y);
%     f = fit(u,v,'smoothingspline');
% 
%     % Instantiate the Best Fit Line (Curve) Object for Customization
%     plt_f = plot(f,u,v);
% 
%     % Customize the Best Fit Line (Curve) Object
%     plt_f(1).Color = 'black';
%     plt_f(2).Color = 'red';
% 
%     % Labels, Titles, and Set Axis to Years (Crime Amt vs. Years Bar Graph)
%     xlabel('Year (1980-2014)');
%     ylabel('Number of murders');
%     title(strcat(colnames(i),' in Indianapolis (1980 to 2015)'))
%     set(gca,'xtick', 1:35, 'xticklabel', 1980:2014, 'Color', 'w'); hold off;
% end
% ---------+----------------+--------------------+---------- %

% -------------------------------------------------------------------------------------------
% Look at several cities inside of each cluster to determine if they make
% sense
% -------------------------------------------------------------------------------------------
raw_crime_data = table2array(T(:,start_crime_stats_index-1:end_crime_stats_index));


% -------------------------------------------------------------------------------------------
% Center the crime data
% Z = D - transpose(mu)*1
% -------------------------------------------------------------------------------------------
% ST_arr = table2array(ST(:,:));
% Z = zeros(numrows,10);
% for i=1:numrows
%      Z(i,:) = raw_crime_data(i,:) - ST_arr(:,2:width(ST));
% end
% -------------------------------------------------------------------------------------------

k=30;
[idxk ,C] = kmeans(raw_crime_data,k,'Distance','sqeuclidean');
% -------------------------------------------------------------------------------------------


% -------------------------------------------------------------------------------------------
% Helper code to see the size of each cluster
% -------------------------------------------------------------------------------------------
for i=1:k
     disp(length(unique(T.city(idxk(:) == i))));   
end
% -------------------------------------------------------------------------------------------

% -------------------------------------------------------------------------------------------
% The following code and notes beneath this point were all bad attempts, but I kept
% because they helped solve certain issues that may be found valuable to learn from
% at later points in the project.
% -------------------------------------------------------------------------------------------

% -------------------------------------------------------------------------------------------
%
% https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate
% 
% Crime quantity by population (More modern):
% 
% New Mexico	Albuquerque	558,874	882.8	5.4	71.9	247.1	558.4	5,446.1	1,095.6	3,713.9	636.6	15.4
% California	Anaheim	346,956	317.3	4.0	22.8	120.5	170.1	2,362.3	375.0	1,619.8	367.5	6.6
% Alaska	Anchorage	301,306	864.6	4.0	130.1	164.6	565.9	3,827.0	456.3	3,059.0	311.6	26.9
% Texas	Arlington	382,976	484.1	3.4	53.8	128.7	298.2	3,515.1	644.9	2,633.6	236.6	6.8
% Georgia	Atlanta	454,363	1,227.4	20.5	33.2	512.6	661.1	5,747.4	1,203.9	3,631.0	912.5	16.5
% Colorado	Aurora	350,948	412.6	3.1	78.1	118.8	212.6	2,838.6	517.5	2,018.0	303.2	21.7
% Texas	Austin	903,924	396.2	3.5	63.2	96.6	232.9	4,142.4	634.2	3,255.0	253.1	10.5
% California	Bakersfield	367,406	456.7	4.6	5.7	179.6	266.7	3,972.4	1,106.4	2,244.7	621.4	101.3
% Maryland	Baltimore	623,513	1,338.5	33.8	39.3	589.7	675.7	4,718.4	1,110.8	2,888.2	719.5	34.2
% Massachusetts	Boston	654,413	725.7	8.1	42.8	256.7	418.1	2,638.9	409.5	1,998.3	231.0	N/A
% New York	Buffalo	258,419	1,228.2	23.2	67.3	494.2	643.5	4,817.4	1,207.0	3,235.8	374.6	N/A
% Arizona	Chandler	252,369	184.7	0.4	23.8	44.0	116.5	2,236.0	378.0	1,767.6	90.3	38.4
% North Carolina	Charlotte-Mecklenburg	856,916	589.8	5.5	24.5	185.1	374.7	3,566.9	703.8	2,663.9	199.2	19.3
% California	Chula Vista	259,894	235.5	2.7	15.4	82.3	135.1	1,740.7	235.9	1,189.3	315.5	13.5
% Illinois	Chicago	2,724,121	884.3	15.1	49.3	359.9	460.0	3,126.2	533.6	2,224.6	367.9	16.9
% Ohio	Cincinnati	297,671	905.4	20.2	76.6	455.5	353.1	5,562.2	1,619.2	3,574.1	368.9	118.6
% Ohio	Cleveland	388,655	1,334.3	16.2	124.0	769.3	424.8	5,434.4	1,787.7	2,659.2	987.5	78.2
% Colorado	Colorado Springs	444,949	458.3	4.5	92.6	90.8	270.4	3,667.6	620.1	2,677.6	369.9	21.1
% Ohio	Columbus	830,811	549.2	10.0	88.8	252.5	197.9	4,253.0	1,091.2	2,807.9	353.9	N/A
% 
% http://www.usatoday.com/story/money/business/2014/11/15/24-7-wall-st-safest-cities/18933099/
% 
% Most Crime Cities paired by Population (For specific 2014, w.r.t population):
% 
% 1. Detroit, Mich.       --> 1,12
% 2. Oakland, Calif.      --> 1,12,14
% 3. Memphis, Tenn.       --> 1,8,12,14
% 4. St. Louis, Mo.       --> 1,8,12,14
% 5. Cleveland, Ohio.     --> 1,8,12,14
% 6. Little Rock, Ark.    --> 8,12
% 7. Baltimore, Maryland. --> 1,12,14
% 8. Rockford, Ill.       --> 1,12,14
% 9. Milwaukee, Wis.      --> 1,12
% 10. Birmingham, Ala.    --> 1,12,14
%
% Least Crime Cities paired by Population:
% 
% 1. Irvine, California --> 1,14
% 2. Murrieta, California --> 1,5
% 3. Cary, North Carolina --> 1,14
% 4. Naperville, Illinois --> 1,14
% 5. Frisco, Texas --> 1,7,14
% 6. Temecula, California --> 1,14
% 7. Gilbert, Arizona --> 1,7,14
% 8. Amherst, New York --> 1,14
% 9. Glendale, California --> 1,5,7,14
% 10. Sunnyvale, California --> 1,14
% -------------------------------------------------------------------------------------------
% -------------------------------------------------------------------------------------------
% 'Bad' Cities
%A = {'DETROIT','OAKLAND','MEMPHIS','ST. LOUIS', 'CLEVELAND','LITTLE ROCK','BALTIMORE','ROCKFORD','MILWAUKEE','BIRMINGHAM'};
% 'Good' Cities
%B = {'IRVINE','MURRIETA','CARY','NAPERVILLE','FRISCO','TEMECULA','GILBERT','AMHERST','GLENDALE','SUNNYVALE'};
% -------------------------------------------------------------------------------------------
%
% -------------------------------------------------------------------------------------------
% For bulk city search analysis -- create array to look into.
% rows are each city % col represents the cluster it showed up in
%disp('BAD CITIES AND COORESPONDING CLUSTERS');
%clust_hits_mat_A=zeros(length(A),k);
%for j = 1:length(A)
%     cluster_hits=zeros(1,k);
%    disp(A{j});
%    for i=1:k
%        if(length(unique(strcmp(unique(T.city(idxk(:) == i)), A{j})))>1)
%            clust_hits_mat_A(j,i)=1;
%             cluster_hits(i) = 1;
%            str = strcat(' cluster: ',int2str(i));
%            disp(str);
%        else
%            clust_hits_mat_A(j,i)=0;
%             cluster_hits(i) = 0;
%    %         disp(strcat('not in cluster: ', int2str(i)))
%        end
%    end
%%     clust_hits_mat(j) = cluster_hits;
%end
%disp(strcat('BADCITIES:NUMBER OF CITIES LANDING IN EACH CLUSTER:',int2str(sum(clust_hits_mat_A(:,:)))));
%disp('GOOD CITIES AND COORESPONDING CLUSTERS');
%clust_hits_mat_B=zeros(length(B),k);
%for j = 1:length(B)
%     cluster_hits=zeros(1,k);
%    disp(A{j});
%    for i=1:k
%        if(length(unique(strcmp(unique(T.city(idxk(:) == i)), B{j})))>1)
%            clust_hits_mat_B(j,i)=1;
%             cluster_hits(i) = 1;
%            str = strcat(' cluster: ',int2str(i));
%            disp(str);
%        else
%            clust_hits_mat_B(j,i)=0;
%             cluster_hits(i) = 0;
%    %         disp(strcat('not in cluster: ', int2str(i)))
%        end
%    end
%     clust_hits_mat(j) = cluster_hits;
%end
%disp(strcat('GOODICITES:NUMBER OF CITIES LANDING IN EACH CLUSTER:',int2str(sum(clust_hits_mat_B(:,:)))));
% -------------------------------------------------------------------------------------------
%
%diffs = sum(clust_hits_mat_A(:,:)) - sum(clust_hits_mat_B(:,:));
%diffs
%
% -----------------------------------------------------------------------------------------------------------
% Graphs representing:
% xaxis:cluster
% yaxis:number of cities landing in it
% Notice the difference in:
% -----------------------------------------------------------------------------------------------------------
%figure();
%subplot(1,2,1);
%bar(sum(clust_hits_mat_A(:,:)),'facecolor','r'); hold on;
%title('Worst Cities');
%xlabel('Cluster #');
%set(gca,'XTick',[1:2:k]);
%ylabel('# of Cities in Cluster'); hold off;
%subplot(1,2,2);
%bar(sum(clust_hits_mat_B(:,:)),'facecolor','b'); hold on;
%title('Best Cities');
%xlabel('Cluster #');
%set(gca,'XTick',[1:2:k]);
%ylabel('# of Cities in Cluster'); hold off;
% -------------------------------------------------------------------------------------------
%
%
% For single city of interst -- create array to look into.
% cluster_hits=zeros(1,k);
% for i=1:k 
%     if(length(unique(strcmp(unique(T.city(idxk(:) == i)), 'FISHERS')))>1)
%         cluster_hits(i) = 1;
%         disp(strcat('in cluster: ',int2str(i)))
%     else
%         cluster_hits(i) = 0;
%     %     disp(strcat('not in cluster: ', int2str(i)))
%     end
% end
% --------------------------------------------------------------------------
%
%
% count=1
% for i = 1:length(cluster_hits)
%     if(cluster_hits(i)==1)
%         figure();
%         uniqueCities = unique(T.city(idxk==i));
%         numberUniqueCities = length(uniqueCities);
%         x = T.city(idxk==i)
% 
%         % For every city -- count how many times it appears in a crime cluster
%         y = zeros(numberUniqueCities);
%         for j = 1:numberUniqueCities
%             y(j) = sum(strcmp(x,x(j)));
%         end
%         y=y(:,1);
% 
%         % Plot!
%         bar(y);
%         ylim([0 50]);
%         set(gca,'Xtick',1:numberUniqueCities,'XTickLabel',uniqueCities, 'Color','w');
%         count = count + 1;
%     else
%     end
% end


% -------------------------------------- + -------------------------------------------------
% I switched to R primarily due to some of the convenient functions and packages available
% to format and clean my data into the following structure:
%
%
%          1979 1980 1981 ... 2014
%         +----+----+----+---+-----
% city1   | C1   C2   C2  ...  C2
% city2   | C5   C2   C2  ...  C5
% .....   | ... ...  ...  ...  ...
% city100 | C10  c7  C10  C7   C7
%
% Next I switch back to MATLAB for a little more control. I run K-means 5 times and calculate
% the purity score of each, finding that K=10 gave me the highest.
%
%
% -------------------------------------- + -------------------------------------------------

