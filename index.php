<html>
<style>
#link_holder{
	text-align:center;
}
#instructionsButton{
position:absolute;
right:98%;
top:5px;
}
#templateButton{
position:absolute;
top:5px;
right:43px;
}
</style>
<head>
<META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE">
<meta charset="utf-8">
<title>B13 Micro</title>
<link rel="stylesheet" type="text/css" href="/js/jquery.mobile-1.0.1/jquery.mobile-1.0.1.css" />
<link rel="stylesheet" type="text/css" href="css/jquery.fileupload-ui.css" />
<link rel="stylesheet" type="text/css" href="css/jquery.mobile.simpledialog.min.css" /> 
<link rel="stylesheet" type="text/css" href="css/b13micro.css" /> 
<link rel="stylesheet" type="text/css" href="/css/exchange/jquery.toastmessage.css" />
</head>
<body>
<div data-role="page" data-theme="b" id="one">
<div data-role="header" data-theme="b" id="mheader" data-position="fixed">
<div id="instructionsButton"> <a href="instructions.php" data-role="button" rel="external">Instructions and Resources</a></div> <h2>B13 Micro</h2>
<div id="templateButton"> <a href="#templateContainer" data-role="button">Plate Setup Templates</a></div>
<a href="#" data-role="button" data-icon="history" data-iconpos="right" id="myhistory">History</a>
</div>
<div data-role="content">
<div id="listhead">
<input id="fileupload" type="file" name="files[]">
</div> <!-- close list head / /-->
<div id="progress_bar"></div> 
<div id="layer_title"></div>
<br>
<div id="filelog"></div>
<div id="layers"></div>
<div id="link_holder">   </div>
<div id="stage"></div>
<div id="signal">
  <ul>Status
    <li id="myfile"><span>File Uploading</span></li>
    <li id="mysub"><span>Collect Information</span></li>
    <li id="myinfo"><span>Information Processing</span></li>
  </ul>
</div>
<!-- <p><a href="#two" data-role="button">Show page "two"</a></p> // -->
</div> <!-- close content // -->
<div data-role="footer" data-theme="b" data-position="fixed"><h2>&nbsp;</h2></div>
</div> <!-- close page one // -->
<div data-role="dialog" data-theme="b" id="two">
	<div data-role="header" data-theme="b">
		<h1>Gather Information</h1>
	</div>
	<div data-role="content" data-theme="b">
		  <div data-role="fieldcontain">
			<div id="myheader"></div>
			   <div id="infoGather">
			    <form>Organization:   <input type="text" id='nameBox' name="nameBox"></form>
				<select id='platformSelect'>
					    <option value="platformNull" selected="selected">Select a Platform</option>
					    <option value="CFX">BioRad CFX</option>
					    <option value="ABI">ABI</option>
				</select>
				<select id='assaySelect'>
					    <option value="assayNull" selected="selected">Select an Assay</option>
					    <option value="ent">Enterococcous Taq Env.</option>
					    <option value="hf183">HF183</option>
				</select>
                	    <a href="#one" data-role="button" data-icon="check" id="submitInfo">Submit</a>
                	    <a href="#one" data-role="button" data-icon="back" id="fail">Cancel</a>
			    </div>
			  </div>
		</div>
	</div>
 </div> <!-- close content // -->
</div> <!-- close page two // -->
<div data-role="dialog" data-theme="b" id="templateContainer">
	<div data-role="header" data-theme="b">
		<h1>Templates</h1>
	</div>
	<div data-role="content" data-theme="b">
		  <div data-role="fieldcontain">
			<div id="myheader"></div>
                	    <a href="plate_setup/bight13_Entero plate setup.pltd" target="_blank" data-role="button" data-icon="arrow-d">CFX Bight13 Entero</a>
                	    <a href="plate_setup/bight13_HF183 duplex plate setup.pltd" target="_blank" data-role="button" data-icon="arrow-d">CFX Bight13 HF183 Duplex</a>
                	    <a href="plate_setup/ABI_StepOne_Bight13_HF183_Plate_Template.eds" target="_blank" data-role="button" data-icon="arrow-d">ABI StepOnePlus Bight13 HF183</a>
                	    <a href="plate_setup/ABI_StepOne_Bight13_Enterococcus_Plate_Template.eds" target="_blank" data-role="button" data-icon="arrow-d">ABI StepOnePlus Bight13 Entero</a>
                	    <a href="plate_setup/7500 EnteroTaqEnviron Template.edt" target="_blank" data-role="button" data-icon="arrow-d">ABI 7500 Bight13 Entero</a>
                	    <a href="plate_setup/7500 HF183 Template.edt" target="_blank" data-role="button" data-icon="arrow-d">ABI 7500 Bight13 HF183</a>
                	    <a href="#one" data-role="button" data-icon="back">Cancel</a>
		</div>
	</div>
 </div> <!-- close content // -->
</div> 
<div data-role="dialog" data-theme="b" id="three">
		<div data-role="header" data-theme="b">
			<h1>History</h1>
		</div>
		<div data-role="content" data-theme="b">
			<div id="history">
			</div>
			<br>
                    <a href="#one" data-role="button" data-icon="back">Back</a>
		</div>
 </div> <!-- close content // -->
</div> <!-- close page three // -->
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
    <script src="js/application.js"></script>
</body>
</html>
