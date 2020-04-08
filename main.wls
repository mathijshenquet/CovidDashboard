#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
scriptPath = ExpandFileName[First[$ScriptCommandLine]];
SetDirectory[DirectoryName[scriptPath]];
ClearAll["Global`*"]


(* ::Input::Initialization:: *)
today=If[TimeObject[]<TimeObject[{14,0,0}],Yesterday,Today];


(* ::Input::Initialization:: *)
europe={"Belgium","Lithuania","Bulgaria","Luxembourg","Cyprus","Malta","Denmark","Netherlands","Germany","Austria","Estonia","Poland","Finland","Portugal","France","Romania","Greece","Slovenia","Hungary","Slovakia","Ireland","Spain","Italy","Czechia","Croatia","Sweden","Latvia","Switzerland","United Kingdom","Norway"};
europe//DeleteDuplicates//Length;


(* ::Input::Initialization:: *)
startDates=Association[#[[1]]->#[[2]]&/@{
{"Europe",1000},
{"China",10},
{"Italy",50},
{"Iran",5},
{"Spain",10},
{"South Korea",50},
{"France",12},
{"United States of America",100},
{"Japan",100},
{"United Kingdom",15},
{"Iraq",5},
{"Switzerland",5},
{"Germany",20},
{"Netherlands",5},
{"Belgium",5},
{"Sweden",5},
{"Austria",20},
{"South Africa",5},
{"Canada",20},
{"Australia",50},
{"Brazil",5},
{"Turkey",5},
{"Indonesia",5},
{"Portugal",5},
{"Philippines",10},
{"Denmark",5},
{"Greece",5},
(*{"Equador",5},*)
{"Egypt",5},
{"Algeria",5},
{"Malaysia",5},
{"Norway",5},
{"Romania",5},
{"India",5},
{"Finland",5},
{"Singapore",10}
}];
countries=Sort[Keys[startDates]];
bedsPer100k=Association@{{"China"->3},
{"Finland"->12.07},
{"Europe"->11.5},
{"Italy"->12.5},
{"Iran"->3720/7000},
{"Spain"->9.7},
{"South Korea"->15},
{"France"->11.6},
{"United States of America"->16},
{"Japan"->4},
{"United Kingdom"->6.6},
{"Belgium"->15.9},
{"Iraq"->1},
{"Switzerland"->33},
{"Germany"->29.2},
{"Netherlands"->6.4},
{"Sweden"->5.8},
{"Austria"-> 29.27},
{"South Africa"->1.5},
{"Canada"->12.9},
{"Australia"->1990/146},
{"Brazil"->6400/2090},
{"Turkey"->27000/808},
{"Indonesia"->2.7},
{"Portugal"->4.3},
{"Philippines"->2.2},
{"Singapore"->5.7},
{"Denmark"->6.7},
{"Greece"->6},
{"Equador"->1},(*Guess*)
{"Egypt"->1},(*Guess*)
{"Algeria"->1},(*Guess*)
{"Malaysia"->3.4},
{"Norway"->8},
{"Romania"->21.4},
{"India"->2.3}
};
(*Impact of non-pharmaceutical interventions (NPIs) to reduce COVID-19 mortality and healthcare demand*)
ifrDist={0.002,0.006,0.03,0.08,0.15,0.6,2.2,5.1,9.3}/100;
hospDist={0.1,0.3,1.2,3.2,4.9,10.2,16.6,24.3,27.3}/100;
critFrac={5,5,5,5,6.3,12.2,27.4,43.2,70.9}/100;
cfrDist={0.0026,0.0148,0.06,0.146,0.3,1.3,4.0,8.6,13.4}/100;
{
{"0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80+"},
hospDist,
(hospDist critFrac),
ifrDist
};
MapThread[Prepend,{%,{"Age group","Hospitalization rate","Critical rate","Fatality rate"}}];
%//TableForm


(* ::Input::Initialization:: *)
Run["cd ./worldometer/; python3 main.py"];
latestRaw=Import["./worldometer/current.csv"]/.{"S. Korea"->"South Korea","USA"->"United States of America","UK"->"United Kingdom",""->0};
latestRaw[[1]];
latest=Association[#[[1]]->{#[[2]],#[[4]],#[[6]]}&/@latestRaw[[2;;]]];
latest[["Europe"]]=Total[(latest/@europe)];
latest["Italy"]


(* ::Input::Initialization:: *)
ageRaw=Import["./age_dist_un.csv"];
fixes={"Republic of Korea"->"South Korea","Iran (Islamic Republic of)"->"Iran"};
proj[key_]:=#[[key]]&;
partition[row_]:=Partition[row[[1;;2 8]],2]~Join~{row[[2 8+1;;2 8+5]]};
parseRow[row_]:=(row[[2]]/.fixes)->Normalize[Total/@partition[row[[5;;]]],Total]
ageData=Association[parseRow/@ageRaw[[5;;]]];
hosp[name_]:=ageData[name].hospDist;
crit[name_]:=ageData[name].(hospDist*critFrac);
ifr[name_]:=ageData[name].ifrDist;
{#}~Join~N[ageData[#]]&/@{"Europe","Italy","Iran","Spain","South Korea","France","United States of America","Japan","United Kingdom","Iraq","Switzerland","Germany","Netherlands","Nigeria","Niger"};
{{"Country", "0-10", "10-20", "20-30","30-40","40-50","50-60","60-70","80-90","80+"}}~Join~%//TableForm
{#,hosp[#],crit[#],ifr[#]}&/@{"China","Italy","Iran","Spain","South Korea","France","United States of America","Japan","United Kingdom","Iraq","Switzerland","Germany","Netherlands","Nigeria","Niger"};
{{"Country", "Hosp. rate", "Crit. rate", "IFR"}}~Join~%//TableForm


(* ::Input::Initialization:: *)
moment:=DateString[{"DayNameShort"," ", "DayShort"," ", "MonthName", " ", "Year",", ", "Hour",":","Minute"}]


(* ::Input::Initialization:: *)
Clear[parse,parseDeaths,parseCases,cases,deaths,allData];
date=DateString[today,{"Year","-","Month","-","Day"}];
raw=Import["https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"<>date<>".xlsx"][[1]]/.{"South_Korea"->"South Korea","United_States_of_America"->"United States of America","United_Kingdom"->"United Kingdom","Czech_Republic"->"Czechia","South_Africa"->"South Africa"};
heading=raw[[1]];
dateIdx=FirstPosition[heading,"dateRep"][[1]];
countryIdx=FirstPosition[heading,"countriesAndTerritories"][[1]];
casesIdx=FirstPosition[heading,"cases"][[1]];
deathsIdx=FirstPosition[heading,"deaths"][[1]];
parseDeaths[{_,lastDeaths_},row_]:={PreviousDate[row[[dateIdx]],"Day"],lastDeaths+row[[deathsIdx]]};
parseCases[{_,lastCases_},row_]:={PreviousDate[row[[dateIdx]],"Day"],lastCases+row[[casesIdx]]};
dates=DateRange[DateObject["01/01/2020"],today];
writeDate[date_]:=DateString[PreviousDate[date,"Day"],{"MonthNameShort"," ","DayShort"}]
times=writeDate/@dates;
fix[data_]:=Max[0,#]&/@Values[TimeSeriesResample[TimeSeries[data],{dates}]];
parse[countryRaw_,key_]:={
FoldList[parseCases,{None,0},Reverse@countryRaw][[2;;]]~Join~{{today,latest[key[[1,1]]][[1]]}},
FoldList[parseDeaths,{None,0},Reverse@countryRaw][[2;;]]~Join~{{today,latest[key[[1,1]]][[2]]}}
};
allData=MapIndexed[parse,
GroupBy[raw[[2;;]],#[[countryIdx]]&]
];
allData[["Italy",1]]=Select[allData[["Italy",1]],#[[1]]!= DateObject["14 March 2020"]&]; (* remove 14th of march, bad data, just linearly extrapolate *)
allData=Map[{fix@#[[1]],fix@#[[2]]}&,allData];
cases["Europe",start_:1]:=Total[cases[#,start]&/@europe];
deaths["Europe",start_:1]:=Total[deaths[#,start]&/@europe];
cases[name_,start_:1]:=allData[[name,1,start;;]];
deaths[name_,start_:1]:=allData[[name,2,start;;]];


(* ::Input::Initialization:: *)
fixes={"Mainland China"->"China","Korea, South"->"South Korea","Iran (Islamic Republic of)"->"Iran","US"->"United States of America"};rawRecovered=Import["https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"]/.fixes;
rTimes=DateObject[{#,{"MonthShort","/","DayShort","/","YearShort"}}]&/@rawRecovered[[1,5;;]];
parseRecoverd[row_]:=Association[{row[[2]]->fix@Transpose[{rTimes,row[[5;;]]/.""->0}]}];
rData=Merge[Map[parseRecoverd,rawRecovered[[2;;]]/.fixes],Total];
recovered["Europe",start_:1]:=Total[recovered[#,start]&/@europe];
recovered[name_,start_:1]:=rData[[name,start;;]];


(* ::Input::Initialization:: *)
fineLogTicks[min_,max_]:=Flatten[Table[Table[d 10^m,{d,0,0.9,0.1}],{m,Floor[min],Ceiling[max]}]];
fineTicks[min_,max_]:=Table[i,{i,0,max,10^(Round[Log[10,max]]-1)}];
denseTicks[min_,max_]:=Table[i,{i,Ceiling[min],Floor[max]}];
spacedTicks[k_]:=Function[{min,max},Table[i,{i,Ceiling[min],Floor[max],k}]];
gray=Lighter[Gray,0.5];
veryLightGray=Lighter[Gray,0.95];
showPercent[num_,p_:1]:=ToString[NumberForm[Round[100*num,p],3]]<>"%";
firstPos[data_,pred_]:=firstPos[data,pred,Length@data];
firstPos[data_,pred_,default_]:=FirstPosition[data,Except[_Symbol]?pred,{default},{1}][[1]];


(* ::Input::Initialization:: *)
Clear[expTrend];
square[x_]:=Power[x,2];
expTrend[x_Symbol,\[Sigma]_,data_,deg_:1,base_:3,norm_:square]:=Block[{cut,model,idxs,weights,decay,f},
cut=firstPos[data,#>0&];
weights=Table[(base/(1+base))^norm[i-2],{i,Length[data]-cut,0,-1}];
f=(x-Length[data]+base);
model=LinearModelFit[
Transpose[{Range[cut,Length@data],Log[data[[cut;;]]]}],
Table[f^i,{i,1,deg}],x,
Weights->weights];
Exp[model["BestFit"]+model["BasisFunctions"][[2]]model["ParameterTableEntries"][[2,2]]\[Sigma]]
];


(* ::Input::Initialization:: *)
data=deaths["Italy"]
trend1=expTrend[x,0,data,1];
trend2=expTrend[x,0,data,2];
cut=firstPos[data,#>1&];
weights=Table[(base/(1+base))^Abs[i-2],{i,Length[data]-cut,0,-1}];
c=D[Log[trend2],x];
Show[{
Plot[{trend1,trend2},{x,cut,Length@data+5},ScalingFunctions->"Log10",PlotRange->All],
ListPlot[data,ScalingFunctions->"Log10"]
},PlotRange->{1,All}
]


(* ::Input::Initialization:: *)
len=Length@times;
Clear[ticks,labeled]
ticks[Infinity,start_:1]:=Table[{i-start,Rotate[Pane[times[[i]]],Pi/3]},{i,len,start,-1}];
ticks[k_,start_:1]:=Table[{i-start,Rotate[Pane[times[[i]]],Pi/3]},{i,len,start,-Ceiling[(len-start)/k]}];
ticks0[k_,start_:1]:=Table[{i-start,Rotate[Pane[times[[i]]],Pi/3],0},{i,len,start,-Ceiling[(len-start)/k]}];
ticksFromNow[k_]:=Function[{min,max},Table[{i,Rotate[Pane[writeDate@DatePlus[today,i]],Pi/3]},{i,Ceiling[min],max,Ceiling[(max-min)/k]}]];
labeled[data_,k_]:=Module[{len=Length@data},Table[If[Divisible[len-i,Ceiling[len/k]],Labeled[data[[i]],data[[i]],Above],data[[i]]],{i,1,len}]];
percentageTicks={
{0.0000001,"1\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-7\)]\)"},{0.0000002,"2\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-7\)]\)"},{0.0000005,"5\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-7\)]\)"},
{0.000001,"1\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-6\)]\)"},{0.000002,"2\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-6\)]\)"},{0.000005,"5\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-6\)]\)"},
{0.00001,"1\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-5\)]\)"},{0.00002,"2\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-5\)]\)"},{0.00005,"5\[CenterDot]\!\(\*SuperscriptBox[\(10\), \(-5\)]\)"},
{0.0001,"0.01%"},{0.0002,"0.02%"},{0.0005,"0.05%"},
{0.001,"0.1%"},{0.002,"0.2%"},{0.005,"0.5%"},
{0.1,"10%"},{0.01,"1%"},{0.02,"2%"},{0.05,"5%"},
{0.2,"20%"},{0.5,"50%"},{1,"100%"}
};
percentages[L_]:={L #[[1]],#[[2]]}&/@percentageTicks;
percentagesLinear[L_,k_:3]:=Function[{min, max},Module[{all,cut},
all={L #[[1]],#[[2]]}&/@percentageTicks;
cut=firstPos[all,#[[1]]>max&]-1;
Take[all,{cut-k,cut}]
]
]
percentagesLinear[10][0,1]


(* ::Input::Initialization:: *)
trendChangeScaling=None;
trendChangeOvershoot=1;
trendChangePlot[name_,n_:10]:=Module[{data,cut,trends,trendsPlot,pointPlot,ticks1},
data=deaths[name];
cut=firstPos[data,#>1&];
ticks1=ticks[Infinity][[1;;len-cut;;Ceiling[(len-cut)/24]]];
trends=Table[expTrend[x,0,data[[1;;i]],1,3],{i,-1,-n,-1}];
trendsPlot=Plot[trends,{x,cut,Length@data+trendChangeOvershoot+1},
ScalingFunctions->scaling,
PlotRange->{{cut,Length@data+trendChangeOvershoot},{1,data[[-1]] 1.5}},
ImageSize->400,
AspectRatio->1,
PlotStyle->Table[Directive[ColorData[97][2],Opacity[.8^(1-i)],If[i==-1,Dotted,Normal]],{i,-1,-n,-1}]
];
pointPlot=ListPlot[data,
PlotStyle->Darker[ColorData[97][4]],
ScalingFunctions->scaling,
PlotRange->{{cut,Length@data},{1,All}}
];
Show[{
trendsPlot,
pointPlot
},
Frame->True,
PlotLabel->"1b. Developing trend, lighter is older",
FrameTicks->{{Automatic,percentagesLinear[population[name]]},{ticks1, None}},
FrameLabel->{{"Deaths",Rotate["as fraction of population",180 Degree]},{None,None}}
]
]
Row[trendChangePlot/@{"Italy","Netherlands","France","Germany"}]


(* ::Input::Initialization:: *)
population["Europe"]:=Total[population/@europe];
population[country_]:=CountryData[country/."Czechia"->"Czech Republic","Population"][[1]];
population["Europe"]


(* ::Input::Initialization:: *)
logit[growth_,x_]:=1/(1+Exp[-growth x]);
invLogit[growth_,y_]:=Log[y-1]/-growth;
critDelay=6;(*8;*)
g[low_,high_,growth_]:=logit[growth,low]-logit[growth,high];
scaled[x0_,d_]:={
Function[x,Piecewise[{
{d x,x<x0},
{d x0+(x-x0),x>=x0}
}]],
Function[y,Piecewise[{
{y/d,y<x0 d},
{y +x0 -d x0 ,y>=x0 d}
}]]
};
Clear[peakInfo];
serialInterval=5;growth=0.18;frac=0.1;
R=Exp[growth serialInterval]/(1-frac) 1
1-1/R
peakInfo[pop_,curr_,len_,growth_,crit_,bedsPer100k_,lostCauseFrac_]:=Module[
{frac,beds,R,L,x0,base,xs,xe,sol,span,casesFrac,duration,excessCases,deaths,capacity},
R=Exp[growth serialInterval]/(1-curr/pop);
frac=1-1/R;
beds=(pop bedsPer100k)/10^5;L=crit*pop*frac;
x0=invLogit[growth,pop/curr]-critDelay; (* the current x *)
base=invLogit[growth,L/(beds .5)];
xs=Min[-2,base-x0]; xe=-base-x0+len; 
span=x/.{
FindRoot[L*g[x+x0,x+x0-len,growth]-beds,{x,base-x0}],
FindRoot[L*g[x+x0,x+x0-len,growth]-beds,{x,-x0-base+len}]
};
duration=span[[2]]-span[[1]];
excessCases=L*g[span[[2]]+x0,span[[1]]+x0,growth]-(duration/len)*beds;
deaths=excessCases+(L-excessCases)*lostCauseFrac;
capacity=beds;
Plot[
{L*g[x+x0,x-len+x0,growth],Labeled[beds,ToString[beds//Round]<>" beds",Scaled[0.9]]},
{x,xs,xe},
ScalingFunctions->"Log10",
PlotRange->{{xs,Min[xe,365]},{beds/2,All}},
PlotLabel->"5. Predicted progression",
PlotLegends->Placed[{"Active patients","Capacity"},Below],
PlotStyle->{ColorData[97][4],Directive[ColorData[97][1],Dashed]},
Frame->True,
Ticks->All,
FrameTicks->{{Automatic,percentages[pop]},{ticksFromNow[20], None}},
ImageSize->Large,
Axes->{False,True},
Epilog->{
Inset[Style[ToString[NumberForm[Round[excessCases],DigitBlock->3,NumberSeparator->" "]]<>" ("<>showPercent[excessCases/L,0.1]<>")",Medium],{(xe+xs)/2,Log[10,capacity]+0.2}],
Inset[Style[ToString[NumberForm[Round[L-excessCases],DigitBlock->3,NumberSeparator->" "]]<>" ("<>showPercent[1-excessCases/L,0.1]<>")",Medium],{(xe+xs)/2,Log[10,capacity]-0.15}],
Inset[Style[ToString[Round[duration]]<>" days",Medium],{(xe+xs)/2,Log[10,capacity]+.65}],
Inset[Style[ToString[Round[span[[1]]]]<>" days",Medium],{span[[1]]/2,Log[10,capacity]+.35}],
Inset[Framed[Style["Estimated current\ncritical cases\n"<>ToString[NumberForm[Round[pop*frac*crit*g[x0,x0-len,growth]],DigitBlock->3,NumberSeparator->" "]]]],Scaled[{1-0.18,0.83}],Background->White],
Inset[Framed[Style["Estimated\nTotal deaths\n"<>ToString[NumberForm[Round[deaths],DigitBlock->3,NumberSeparator->" "]]]],Scaled[{0.18,0.83}],Background->White],
Arrowheads[{-.02,.02}],Arrow[{
{span[[1]],Log[10,capacity]+0.5},
{span[[2]],Log[10,capacity]+0.5}
}],Arrow[{
{0,Log[10,capacity]+.2},
{span[[1]],Log[10,capacity]+.2}
}]
},
Filling->{1->{{2},{None,Directive[ColorData[97][4],Opacity[0.2]]}}},
FrameLabel->{{"Critical cases (absolute)", Rotate["fraction of population",180 Degree]},{"Days from now",None}}
]
];
name="Finland";estIFR=0.0066;estCrit=crit[name];cur=80 10^3;pop=population[name];
obsGrowth=0.2;
peakInfo[pop,cur,10,obsGrowth,estCrit,10*bedsPer100k[name],estIFR/estCrit]
peakInfo[pop,cur,10,obsGrowth,estCrit,bedsPer100k[name],ifr[name]/estCrit]


(* ::Input::Initialization:: *)
18.8/14.4


(* ::Input::Initialization:: *)
peakInfo[pop_,curr_,len_,growth_,frac_crit _,bedsPer100k_,lostCauseFrac_]:=Module[
{beds,R,L,x0,base,xs,xe,sol,span,casesFrac,duration,excessCases,deaths,capacity},
R=Exp[growth serialInterval]/(1-curr/pop);
beds=(pop bedsPer100k)/10^5;L=crit*pop*frac;
x0=invLogit[growth,pop/curr]-critDelay; (* the current x *)
base=invLogit[growth,L/(beds .5)];
xs=Min[-2,base-x0]; xe=-base-x0+len; 
span=x/.{
FindRoot[L*g[x+x0,x+x0-len,growth]-beds,{x,base-x0}],
FindRoot[L*g[x+x0,x+x0-len,growth]-beds,{x,-x0-base+len}]
};
duration=span[[2]]-span[[1]];
excessCases=L*g[span[[2]]+x0,span[[1]]+x0,growth]-(duration/len)*beds;
deaths=excessCases+(L-excessCases)*lostCauseFrac;
capacity=beds;
Plot[
{L*g[x+x0,x-len+x0,growth],Labeled[beds,ToString[beds//Round]<>" beds",Scaled[0.9]]},
{x,xs,xe},
ScalingFunctions->"Log10",
PlotRange->{{xs,Min[xe,365]},{beds/2,All}},
PlotLabel->"5. Predicted progression",
PlotLegends->Placed[{"Active patients","Capacity"},Below],
PlotStyle->{ColorData[97][4],Directive[ColorData[97][1],Dashed]},
Frame->True,
Ticks->All,
FrameTicks->{{Automatic,percentages[pop]},{ticksFromNow[20], None}},
ImageSize->Large,
Axes->{False,True},
Epilog->{
Inset[Style[ToString[NumberForm[Round[excessCases],DigitBlock->3,NumberSeparator->" "]]<>" ("<>showPercent[excessCases/L,0.1]<>")",Medium],{(xe+xs)/2,Log[10,capacity]+0.2}],
Inset[Style[ToString[NumberForm[Round[L-excessCases],DigitBlock->3,NumberSeparator->" "]]<>" ("<>showPercent[1-excessCases/L,0.1]<>")",Medium],{(xe+xs)/2,Log[10,capacity]-0.15}],
Inset[Style[ToString[Round[duration]]<>" days",Medium],{(xe+xs)/2,Log[10,capacity]+.65}],
Inset[Style[ToString[Round[span[[1]]]]<>" days",Medium],{span[[1]]/2,Log[10,capacity]+.35}],
Inset[Framed[Style["Estimated current\ncritical cases\n"<>ToString[NumberForm[Round[pop*frac*crit*g[x0,x0-len,growth]],DigitBlock->3,NumberSeparator->" "]]]],Scaled[{1-0.18,0.83}],Background->White],
Inset[Framed[Style["Estimated\nTotal deaths\n"<>ToString[NumberForm[Round[deaths],DigitBlock->3,NumberSeparator->" "]]]],Scaled[{0.18,0.83}],Background->White],
Arrowheads[{-.02,.02}],Arrow[{
{span[[1]],Log[10,capacity]+0.5},
{span[[2]],Log[10,capacity]+0.5}
}],Arrow[{
{0,Log[10,capacity]+.2},
{span[[1]],Log[10,capacity]+.2}
}]
},
Filling->{1->{{2},{None,Directive[ColorData[97][4],Opacity[0.2]]}}},
FrameLabel->{{"Critical cases (absolute)", Rotate["fraction of population",180 Degree]},{"Days from now",None}}
]
];


(* ::Input::Initialization:: *)
mean=14.5;median=13.2;
mu=Log[median];sigma=Sqrt[2*(Log[mean]-Log[median])];
onsetToDeath1=LogNormalDistribution[mu,sigma];
mean=17.8;cv=0.45;var=(cv*mean)^2;
onsetToDeath2=GammaDistribution[mean*mean/var,var/mean];
onsetToDeath=onsetToDeath2;
Plot[{PDF[onsetToDeath1,x],PDF[onsetToDeath2,x]}//Evaluate,{x,1,30},Filling->Axis]
computeDelay[growth_]:=NIntegrate[x PDF[onsetToDeath,x] growth^-x,{x,0,Infinity}]/NIntegrate[PDF[onsetToDeath,x] growth^-x,{x,0,Infinity}];
computeDelay[1.3]


(* ::Input::Initialization:: *)
Clear[overviewPlot]
overviewPlot[name_,start_]:=Show[{
ListLinePlot[{cases[name,start]-recovered[name,start],cases[name,start],deaths[name,start]},
PlotRange->{{1,len-start+1},{0,All}},
Frame->True,
ScalingFunctions->None,
PlotStyle->{ColorData[97][2],ColorData[97][3],ColorData[97][4]},
GridLines->{If[len-start>62,spacedTicks[7],denseTicks],fineTicks},
GridLinesStyle->{{gray,Dotted},gray},
ImageSize->Large,
AspectRatio->1,
Filling->{2->{1},1->{3},3->Bottom},
FrameLabel->{{"Reported cases",Rotate["fraction of population",180 Degree]},{None,None}},
(*LabelingFunction\[Rule](#1&),*)
PlotMarkers->Automatic,
FrameTicks->{{fineTicks,percentagesLinear[population[name]]},{ticks0[32,start],None}},
PlotLabel->Row[{"1. Reported (cumulative) ", LineLegend[
{ColorData[97][2],ColorData[97][3],ColorData[97][4],Gray},
{"Active","Recovered","Deaths","Daily cases"},
LegendLayout->"Row"
]}]
],
BarChart[
{0}~Join~(cases[name,start][[2;;-1]]-cases[name,start][[1;;-2]]),
PlotRange->{All,{0,All}},
ChartStyle->{Opacity[0]},
Frame->True,
BarSpacing->None
]/.pt:{x_Real,y_Real}:>{x-.5,y}
}];
name="United States of America";start=65;
overviewPlot[name,start]


(* ::Input::Initialization:: *)
intervals={1,0.5,0.2,0.1,0.05,0.02,0.01,0.005};
cfrMults={0.25,0.5,2,4};
casesPlot[name_,skip_:1]:=Module[{mainPlot,projPlot,start,min,retroPlot,deathTrend,deathTrend2,growth,delay,trend,trend2,t,curr},
deathTrend=expTrend[t,0,deaths[name]];
deathTrend2=expTrend[t,0,deaths[name],2];
growth=Coefficient[deathTrend[[2]],t];
delay=computeDelay[Exp[growth]]//Round;
start=Max[skip,firstPos[deaths[name],#>0&]-delay]//Floor;
trend=deathTrend/(ifr[name]cfrMult)/.t->t+delay+start-1;
trend2=deathTrend2/(ifr[name]cfrMult)/.t->t+delay+start-1;
min=Min[
deaths[name,start][[1+delay]]/ifr[name]
];
mainPlot=ListLinePlot[Table[cases[name,start]/i,{i,intervals}],
Frame->True,
PlotLabels->Placed[
Table[Rotate[TextString[100*i]<>"%",20 Degree],{i,intervals}],
{Scaled[1],Below}
],
PlotStyle->{ColorData[97][1]}~Join~Table[Lighter[ColorData[97][1],0.5],{i,intervals[[2;;]]}],
PlotRange->{{1,len-start+1},{min,trend/.{t->len-start,cfrMult->First[cfrMults]*0.8}}},
ScalingFunctions-> "Log10",
GridLines->{denseTicks,fineLogTicks},
GridLinesStyle->{{gray,Dotted},LightGray},
(*Filling\[Rule]{1\[Rule]Bottom,2\[Rule]Bottom,3\[Rule]Bottom,4\[Rule]Bottom,5\[Rule] Bottom},*)
ImageSize->Large,
AspectRatio->1,
FrameTicks->{{Automatic,percentages[population[name]]},{ticks[32,start],None}}
];
projPlot=Show[{
Plot[
Table[trend,{cfrMult,cfrMults}],
{t,len-start-delay-3-15,len-start+5},
ScalingFunctions->"Log10",
PlotStyle->Directive[ColorData[97][2],AbsoluteThickness[0.8]],
ImageSize->Large,
PlotRange->Full,
AspectRatio->1
],
(*Plot[
Table[trend2,{cfr,cfrs}],
{t,len-start-delay-3,len-start-delay+1},
ScalingFunctions\[Rule]"Log10",
PlotStyle\[Rule]Directive[ColorData[97][2],AbsoluteThickness[0.8],Dashed],
ImageSize\[Rule]Large,
PlotRange\[Rule]Full,
AspectRatio\[Rule]1
],*)
Plot[
trend/.cfrMult->1,
{t,len-start-delay-3-15,len-start+5},
ScalingFunctions->"Log10",
PlotStyle->Directive[ColorData[97][2],AbsoluteThickness[1]],
ImageSize->Large,
PlotRange->Full,
AspectRatio->1
]
}];
retroPlot=Show[{ListLinePlot[
Table[deaths[name,start][[delay+1;;]]/(ifr[name]cfrMult),{cfrMult,cfrMults}],
ScalingFunctions-> "Log10",
PlotLabels->Placed[
Table[Rotate[showPercent[ifr[name]*cfrMult,0.01],20 Degree],{cfrMult,cfrMults}],
{len-start-delay,Below}
],
PlotStyle->Directive[ColorData[97][4],Opacity[0.6]],
PlotRange->Full
],
ListLinePlot[
deaths[name,start][[delay+1;;]]/ifr[name],
ScalingFunctions-> "Log10",
PlotLabels->Placed[
{Rotate[showPercent[ifr[name],0.01],20 Degree]},
{len-start-delay,Below}
],
PlotStyle->ColorData[97][4],
PlotRange->Full
]}];
Show[{
mainPlot,
(*projErrorPlot,*)
projPlot,
retroPlot
},
ImageMargins->{{20,0},{0,0}},
Epilog->Inset[Framed[Style["Estimated daily growth "<>showPercent[growth,0.1]]],Scaled[{0.2,0.95}],Background->White],
FrameLabel->{{"Estimated cases",Rotate["as fraction of population",180 Degree]}, {None, None}},
PlotLabel->Row[{"2. Estimation of cases"," ",LineLegend[{ColorData[97][1],ColorData[97][4]},{"using reported cases, by percentage of known cases (labeled)","using deaths, by case fatality rate (labeled)"}]}]
]
];
Row[{casesPlot["Turkey"],casesPlot["China"]}]


(* ::Input::Initialization:: *)
smallSize=400;


(* ::Input::Initialization:: *)
offsetPlot[dx_,dy_:0]:=Function[{plot},
plot/.pt:{x_Real,y_Real}:>TranslationTransform[{dx,dy}]@pt
];
deathPlot[name_]:=Module[{ds,start,progessPlot,deltaPlot1,deltaPlot2,deltas,y0,y1},
start=Max[firstPos[deaths[name],#>1&],len-12];
ds=deaths[name,start];
deltas=ds[[2;;-1]]-ds[[1;;-2]];
y0=(Max[0,Min[deltas,ds[[1]]]]+1)/2;
progessPlot=ListLinePlot[
MapIndexed[Callout[#1,#1]&,ds],
AspectRatio->1,
ImageSize->smallSize,
FrameLabel->{{"Deaths",Rotate["as fraction of population",180 Degree]},{None, None}},
PlotRange->{{2,len-start+1},{y0,All}},
GridLinesStyle->{{gray,Dotted},LightGray},
PlotStyle->ColorData[97][4],
Frame->True,
ScalingFunctions->"Log10",
FrameTicks->{{Automatic,percentages[population[name]]},{ticks0[12,start], None}},
GridLines->{denseTicks,fineLogTicks},
GridLinesStyle->{{gray,Dotted},LightGray}
];
deltaPlot1=BarChart[
{0}~Join~deltas,
PlotRange->{Automatic,{y0,Automatic}},
ChartLabels->Placed[Round/@{0}~Join~deltas,Top],
ChartStyle->{Opacity[0]},
Frame->True,
ScalingFunctions->"Log10",
PlotRange->{{1,Length@ds-1},{ds[[1]],10^4}},
BarSpacing->None
];
deltaPlot2=deltaPlot1/.pt:{x_Real,y_Real}:>{x-.5,y};
Show[{
progessPlot,
deltaPlot2
},PlotLabel->Row[{"1a. Recent ",
LineLegend[
{ColorData[97][4],Gray},
{"Total deaths","Daily deaths"},
LegendLayout->"Row"
]
}]
]
]
Row[deathPlot/@{"Italy","China"}]


(* ::Input::Initialization:: *)
Clear[infos];
ventDur=10;
infos[name_String]:=Module[{innerStart,delay,rs,cs,ds,len,tickLabels,growth,peakPlot,popTicks,intervals,max,casesPlot2,deathTrend,deathTrend2,trend,trend2,estNow,trends,popPlot,t,projErrorPlot,projPlot,retroPlot,cfrs,epiPlot,estIFR,estCrit,skip},
skip=startDates[name];
estIFR=ifr[name];
estCrit=crit[name];
cs=cases[name];
innerStart=Max[1,firstPos[cs,#>skip&]-1];
cs=cs[[innerStart+1;;]];ds=deaths[name][[innerStart+1;;]];rs=recovered[name][[innerStart+1;;]];len=Length@cs;
tickLabels=Table[{i-1,Rotate[Pane[times[[innerStart+i]]],Pi/3]},{i,1,len}];
intervals={1,0.5,0.2,0.1,0.05,0.02,0.01,0.005};
max=Max[1.5*ds[[-1]]/intervals[[-1]],1.1*cs[[-1]]];
cfrs={0.02,0.04,0.08,0.01,0.005};
deathTrend=expTrend[t,0,ds];
deathTrend2=expTrend[t,0,ds,2];
growth=Coefficient[deathTrend[[2]],t];
delay=computeDelay[Exp[growth]]//Round;
trend=deathTrend/cfr/.t->t+delay;
trend2=deathTrend2/cfr/.t->t+delay;
estNow=trend/.{cfr->estIFR,t->len};
popTicks={
{0.00001,"0.001%"},{0.00002,"0.002%"},{0.00005,"0.005%"},
{0.0001,"0.01%"},{0.0002,"0.02%"},{0.0005,"0.05%"},
{0.001,"0.1%"},{0.002,"0.2%"},{0.005,"0.5%"},
{0.1,"10%"},{0.01,"1%"},{0.02,"2%"},{0.05,"5%"},
{0.2,"20%"},{0.5,"50%"},{1,"100%"}
};
epiPlot=ListLinePlot[
Table[cs[[1;;-delay]]ifr[name] cfrMult/(1+ds[[delay;;]]),{cfrMult,{1}~Join~cfrMults}],
PlotLabels->Placed[Table[showPercent[ifr[name] cfrMult,0.01],{cfrMult,{1}~Join~cfrMults}],{Scaled[0.6],Below}],
PlotLabel->"3. Percentage of known cases, by CFR",
AspectRatio->1,
ImageSize->smallSize,
PlotRange->{{1,Length@ds-delay+1},{0.01,1}},
GridLines->{denseTicks,{0.02,0.04,0.06,0.1,0.2,0.4,0.6}},
GridLinesStyle->{{gray,Dotted},LightGray},
PlotStyle->{ColorData[97][4]}~Join~Table[Lighter[ColorData[97][4],0.5],{cfr,cfrs[[2;;]]}],
Frame->True,
FrameTicks->{{{0.02,"2%"},{0.04,"4%"},{0.06,"6%"},{0.1,"10%"},{0.2,"20%"},{0.4,"40%"},{0.6,"60%"}},{tickLabels[[1;;-delay;;Ceiling[len/16]]],None}},
ScalingFunctions->"Log10",
FrameLabel->"Percentage of known cases",
ImagePadding->{{60,60},{50,5}}
];
peakPlot=peakInfo[population[name],estNow,ventDur,growth,estCrit,bedsPer100k[name],estIFR/estCrit];
Column[{
Row[{
Style[name <> " ","Subtitle",Black],
Style["COVID-19 situation report for "<>moment<>" see https://t.me/covid_dashboard for the latest" ,"Text",14]
}],
Row[{
overviewPlot[name,innerStart],
casesPlot[name,innerStart]
}],
Row[{
deathPlot[name],
trendChangePlot[name],
epiPlot
}],
TextCell["Note: Data for the last day is just an estimate and might not reflect the numbers. Description: 1. Cumulative plot of reported active cases, recovered cases and deaths. 1a. Recent deaths (red lines) and daily deaths (bars). 1b. Historical estimates of the exponential growth (yellow lines) in the reported deaths (red dots) are displayed, more faded lines represent older estimates (dotted line is today). Indicates whether growth is speeding up or slowing down. 2. Using case fatality rate (CFR) estimates (see methods), we retrodict the amount of cases in the past (red) and predict an exponential trend (yellow). We also estimate the amount of cases depending on which percentage of cases is reported (blue lines). 3. Retrodicted cases using are divided by reported cases. Used to estimate how good testing is. Methods: With the current growth of "<>showPercent[growth,0.1]<>" per day a recorded death is expected to have started showing symptoms "<>ToString[delay]<>" days ago (onset-to-death is Gamma distributed with mean 18 days and coefficient of variation 0.45, doi:10.1101/2020.03.09.20033357). We use age dependent CFR data (doi:10.1101/2020.03.09.20033357) and the demographics of "<>name<>" to calculate the expected CFR to be "<>showPercent[ifr[name],0.01]<>". Sources: ECDC for historical data and worldometer for last day.",
"SmallText",CellSize->{1200,170}],
Row[{peakPlot,TextCell["5. We use a logistic model to estimate the progression of the epidemic using, current growth of "<>showPercent[growth,0.1]<>", estimating current cases using a "<> showPercent[estIFR,0.01] <>", that "<> showPercent[estCrit,0.01] <>" of cases become critical (needing ventilation), that the average critical case needs "<>ToString[ventDur] <>" days of ventilation, that "<> showPercent[estIFR/estCrit] <>" of patients die even with ventilation, and using the amount of ICU beds as estimate for the amount of ventilators in a country. These numbers are derived from doi:10.1101/2020.03.09.20033357 and the publicly availible information on demographics and IC beds in "<>name ,"SmallText",CellSize->{600,160}]},Spacer[10],Alignment->Bottom]
}]
];


(* ::Input::Initialization:: *)
doExport[name_]:=Block[{fileName},
fileName="./out/"<>name<>"-"<>date<>".png";
Export[fileName,Row[{Spacer[5],infos[name],Spacer[5]},ImageSize->1211]];
(*RunProcess[{"telegram-cli","-e", "channel_list\nsend_document @covid_dashboard "<>fileName}];*)
]
DeleteDirectory["./out",DeleteContents->True];CreateDirectory["./out"];
doExport/@countries
