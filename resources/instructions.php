<html>
<style>
#link_holder{
	text-align:center;
}
#backButton{
position:absolute;
right:99%;
top:5px;
}
#templateButton{
position:absolute;
top:5px;
right:20px;
}
#collapse{
margin-left:14px;
margin-right:60%;
width:238px;
}
p { margin: 25px; }
ol li { margin: 0 25px 25px 25px; }
ol.alpha { color: #0f0f0f; }
</style>
<head>
<meta charset="utf-8">
<title>Instuctions and Resources</title>
<link rel="stylesheet" type="text/css" href="/js/jquery.mobile-1.0.1/jquery.mobile-1.0.1.css" />
<link rel="stylesheet" type="text/css" href="css/jquery.fileupload-ui.css" />
<link rel="stylesheet" type="text/css" href="css/jquery.mobile.simpledialog.min.css" /> 
<link rel="stylesheet" type="text/css" href="css/b13micro.css" /> 
<link rel="stylesheet" type="text/css" href="/css/exchange/jquery.toastmessage.css" />
</head>
<body>
<div data-role="page" data-theme="b" id="one">
<div data-role="header" data-theme="b" id="mheader" data-position="fixed">
<div id="backButton"> <a href="index.php" data-role="button" data-icon="back"  rel="external">Back</a></div> <h2>Instructions and Resources</h2>
<div id="templateButton"> <a href="http://i.msdn.microsoft.com/hh335072.Platt_Clippy_hires%28en-us,MSDN.10%29.jpg" data-role="button">Further Assistance</a></div>
<a href="#" data-role="button" data-icon="history" data-iconpos="right" id="myhistory">History</a>
</div>

<div data-role="collapsible" data-theme="b" data-mini="true" id="collapse" data-content-theme="b">
   <h4>Additional Resources</h4>
	<a href="plate_setup/qPCR SOP_EMM_HF183 OSTD2 20130819 v1_1_final.php" data-role="button" >qPCR SOP EMM HF183 OSTD2</a>
                	    <a href="resources/qPCR SOP_SCCWRP_Emm Ent Sketa_CFX96_20130819 v1_1_final.php" data-role="button" >qPCR SOP Emm Ent Sketa CFX96</a>
                	    <a href="plate_setup/qPCR SOP_SCCWRP_Emm Ent Sketa_CFX96_20130821 v1_1 final cheat sheet.php" data-role="button" >qPCR SOP Emm Ent Sketa CFX96 Cheat Sheet</a>
                	    <a href="plate_setup/qPCR SOP_EMM_HF183 OSTD2 20130819 v1_1 final cheat sheet.php" data-role="button" >qPCR SOP EMM HF183 OSTD2 Cheat Sheet</a>
                	    <a href="plate_setup/bight13_Entero plate setup.pltd" target="_blank" data-role="button" >ABI 7500 Bight13 Entero</a>
                	    <a href="plate_setup/bight13_Entero plate setup.pltd" target="_blank" data-role="button" >ABI 7500 Bight13 HF183</a>
 
</div>

<p align="center">
    <b>Overview </b>
</p>


<p>
    This tool is designed to take output from qPCR assay run on a Bio-Rad CFX96, ABI StepOnePlus, or ABI 7500 qPCR platform, perform automatic quality control assessment, and target
    concentration calculation (<i>Enterococcus</i> or HF183), and return all results in a pdf report. The quality control assessments include performance of standard curves,
    negative controls, and sample processing and inhibition flagging based on the salmon control assay (and/or the internal amplification control assay).
</p>
<p>
    In order to successfully analyze your data, you will need to do the following:
</p>
<p>
    1) Format your plate setup using your platform's software package. To do this, download the plate setup template provided here for your particular platform 
	and assay. From the template, you should only need to enter the names of your samples to complete the plate setup. 
</p>
<p>
    2) Export the results of the qPCR reaction from the Bio-Rad lab manager (or equivalent software on your qPCR platform) into a comma separated values (csv) format file
</p>
<p>
    3) Drag and drop the csv file over the front page of this tool (or use the browse and uploading function). Assuming there are no errors, you will then be able to view and download the results of the
    analysis.
</p>
<br>
<br>

<p align="center">
    <b>Detailed Instructions</b>
</p>
<p>
    1) Select the correct assay:
</p>
<p>
    <i>Enterococcus</i> Taq Environmental: The plate should be divided in equal parts between reactions targeting <i>Enterococcus</i> (label the "Target" as "ent"), and the other targeting
    the sketa sample processing control (label the "Target" as "sketa"). Each sample should have at least one "ent" reaction and at least one "sketa" reaction
(duplicates are recommended). Additionally, four point standard curves with 10 fold dilutions from 10<sup>5</sup> to 10<sup>2</sup> are required for both targets. These are recommended to consist of a dilution series from 10    <sup>5</sup> to 10<sup>1</sup> copies/&#181;l, with a dilution factor of 10, and each standard reaction duplicated. Importantly, the calibrator must be labeled
    as "calibrator" in the "Sample" column (usually the 10<sup>5</sup> dilution). Finally, each plate should contain duplicate NTC (qPCR blank) and NEC (extraction
    blank). These well must be marked as "NTC" and "NEC" in the "Sample" column.
</p>
<p>
    HF183: The sketa side of the plate can be set up in the same way as the <i>Enterococcus</i> assay, except that for both sketa and HF183 NTC and
	NEC wells will be in triplicate, rathern than duplicate. HF183 requires a six point standard curve with 
	10 fold dilutions from 10<sup>6</sup> to 10<sup>1</sup> copies per 2 ul. Every HF183 reaction well should be a duplex with an internal amplification control (IAC). Even though every well with
	IAC should have the same amount of IAC material in it, wells used in the standard curve should label the same starting quantity for both targets.
	 
</p>
<p>
2) Export the data. For the CFX platform, under export, select "custom export". In the window, select the export format to be CSV. Check to make sure that at least the following columns will be exported:
    Target Name, Content, Sample Name, Cq, Starting Quantity. Make sure that "export experimental information" is selected. Additional exported columns will
    not affect this tool. For ABI platforms, click export, and make sure the result box is checked. Change the output format from xls to text, and in the options tab change
	the delimiter is set to commas. Export all columns; in particular, the seventh column of the output must be the Ct column. 
</p>
<p>
    3) Send the data to the tool. This can be accomplished by 1) finding the icon of the exported CSV file (e.g., on the desktop), and dragging it over the
    "drag here" icon of the front page of this tool, or 2) by clicking the browse button and selecting the exported CSV in your file system.
</p>
<br>
<br>

<p align="center">
    <b>Potential Solutions to Errors</b>
</p>
<p>
    1) Check that your input file is a .csv file.
</p>
<p>
    1) Check that your samples are labeled the same way for <i>Enterococcus</i> and Sketa assays.
</p>
<p>
    2) Check that the calibrator (if <i>Enterococcus</i> assay) is labeled correctly.
</p>
<p>
    3) Check that the NTC and NEC are labeled correctly.
</p>
</div>


 </div> <!-- close content // -->
		</div>
    <script src="/js/jquery-1.6.4/jquery.min.js"></script>
    <script src="/js/jquery-1.6.4/jquery-ui.min.js"></script>
    <script src="/js/jquery-1.6.2/ui/jquery.ui.core.js"></script>
    <script src="/js/jquery-1.6.2/ui/jquery.ui.widget.js"></script>
    <script src="/js/jquery-1.6.2/ui/jquery.ui.progressbar.js"></script>
    <script src="/js/jquery-1.6.2/ui/jquery.effects.core.js"></script>
    <script src="/js/jquery-1.6.2/ui/jquery.effects.pulsate.js"></script>
    <script src="/js/jquery.iframe-transport.js"></script>
    <script src="/js/jquery.fileupload.js"></script>
    <script src="/js/jquery.loader-0.3.js"></script>
    <script src="/js/jquery.mobile-1.0.1/jquery.mobile-1.0.1.min.js"></script>
    <script src="/js/jquery-1.6.4/jquery.mobile.simpledialog2.min.js"></script>
    <script src="/js/exchange/jquery.toastmessage.js"></script>
    <script src="js/applicationInstructions.js"></script>
</body>
</html>
