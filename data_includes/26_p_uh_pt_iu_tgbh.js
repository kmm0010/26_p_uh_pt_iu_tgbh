
var answerHashTable = {};
answerHashTable["pt"] = ["P", "T"];
answerHashTable["tp"] = ["T", "P"];


function getRandomSubarray(arrx, sizex) {
    var shuffled = arrx.slice(0), i = arrx.length, temp, index;
    while (i--) {
        index = Math.floor((i + 1) * Math.random());
        temp = shuffled[index];
        shuffled[index] = shuffled[i];
        shuffled[i] = temp;
    }
    return shuffled.slice(0, sizex);
}


var stimgrouplist = ['pt', 'tp'];
var stimgroup = getRandomSubarray(stimgrouplist, 1)[0];


define_ibex_controller({
	name: "Trial",
	jqueryWidget: {
		_init: function () {
			this.options.transfer = null; // Remove ’click to continue message’.
			this.options.hideProgressBar = true; 
			this.element.VBox({
				options: this.options,
				triggers: [1],
				children: [
					"Message", this.options,
					"Question", this.options,
				]
			});
		}
	},
	properties: { }
});


var defaults = [
    "AcceptabilityJudgment", { 
		presentAsScale: true, 
		hideProgressBar: true },
    "Question", { 
		as: answerHashTable[stimgroup], //
//		instructions: "Which sound?", 
		hasCorrect: false, 
		showNumbers: false,
		randomOrder: false, 
		presentAsScale: true, 
		timeout: 4350, 
		hideProgressBar: true },
    "Separator", { transfer: 500, normalMessage: "", errorMessage: "", hideProgressBar: true },
    "Message", { hideProgressBar: true },
    "Trial", { hideProgressBar: true},
    "Form", { hideProgressBar: true} //,

];


var shuffleSequence = seq(
	"intro", 
	//"conditionbutton",
	//"answerlist",
	"questionnaire",
	"headphones", 
	"soundcheck", 
	"instructions", 
	"instructionsSpeed", 
	"practiceIntro", 
	"practiceInstruct",
	
	"ready",
	"herewego",
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('pnt_')) )), 

	"practiceTimeout",
	
	"ready",
	"herewego",
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith("pto_")) )), 
	"thatsitpractice",
	
	"noAnswer", 
	"ready",
	"herewego",

	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	"pause",
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	"pause",
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),

	"midbreak",

	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	"pause",
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	"pause",
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),
	sepWith("ITI", precedeEachWith(seq("cross", "aftercross"), randomize( startsWith('t_') )) ),

	"debrief",
	"exit2"
);




var items = [



["conditionbutton", "Separator", {transfer: "keypress", normalMessage: stimgroup, hideProgressBar: true, ignoreFailure: true}],
["answerlist", "Separator", {transfer: "keypress", normalMessage: answerHashTable[stimgroup].join(' : '), hideProgressBar: true, ignoreFailure: true}],


// Messages to subject

// Introduction to experiment - the very first screen the subject sees
["intro", "Form", {html: {include: "A_intro.html"}, checkedValue: stimgroup.concat( answerHashTable[stimgroup].join(',') ), saveReactionTime: true} ],


// A linguistic history questionnaire
["questionnaire", "Form", {html: {include: "B_questionnaire.html"}, saveReactionTime: true} ], 

["headphones", "Form", {html: {include: "C_headphones.html"}, saveReactionTime: true} ],

// Instructions
["soundcheck", "Message", {transfer: "keypress", html: {include: "D_soundcheck.html"} } ],
["instructions", "Message", {transfer: "keypress", html: {include: "E_instructions.html"} } ],
["instructionsSpeed", "Message", {transfer: "keypress", html: {include: "F_instructionsSpeed.html"} } ],

// Practice
["practiceIntro", "Message", {transfer: "keypress", html: {include: "G_practiceIntro.html"} } ],
["practiceInstruct", "Message", {transfer: "keypress", html: {include: "H_practiceInstruct.html"} } ],
["practiceTimeout", "Message", {transfer: "keypress", html: {include: "I_practiceTimeout.html"} } ],


// Tells subject that they are done with the practice items
["thatsitpractice", "Message", {transfer: "keypress", html: {include: "J_practiceDone.html"} } ],
["noAnswer", "Message", {transfer: "keypress", html: {include: "K_noAnswer.html"} } ],
["ready", "Message", {transfer: "keypress", html: {include: "_ready.html"} } ],
["herewego", "Message", {transfer: 600, html: {include: "_go.html"} } ],

["pause", "Message", {transfer: "keypress", html: {include: "_pause.html"} } ],
["midbreak", "Message", {transfer: "keypress", html: {include: "_break.html"} } ],


// debriefing
["debrief", "Form", {html: {include: "Y_debrief.html"},
                  saveReactionTime: true
                 } ], 


// Creates unique ID to paste into AMT recruitment page
["exit2", "Form", {html: { include: "Z_exit2.html" },
                   requiresConsent: true,
				   continueMessage: "Press here to conclude the experiment.",
                   saveReactionTime: true
			      } ],

["aftercross", "Separator", {transfer: 210, normalMessage: "", hideProgressBar: true, ignoreFailure: true}],
["ITI", "Separator", {transfer: 700, normalMessage: "", hideProgressBar: true, ignoreFailure: true}],
["cross", "Separator", {transfer: 475, normalMessage: "+", hideProgressBar: true, ignoreFailure: true}],


// Begin experimental items

// practice trials
// no timeout

['pnt_b_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: null, hideProgressBar: true}, "Separator", 150, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> P </div>", transfer: 1900}],
['pnt_b_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: null, hideProgressBar: true}, "Separator", 150, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> T </div>", transfer: 1900}],
['pnt_sp_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: null, hideProgressBar: true}, "Separator", 150, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> P </div>", transfer: 1900}],
['pnt_sp_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: null, hideProgressBar: true}, "Separator", 150, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> T </div>", transfer: 1900}],


// practice trials
// with timeouts
['pto_sp_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: 4000, hideProgressBar: true}, "Separator", 125, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> P </div>", transfer: 1500}],
['pto_sp_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: 4000, hideProgressBar: true}, "Separator", 125, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> T </div>", transfer: 1500}],
['pto_b_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: 4000, hideProgressBar: true}, "Separator", 125, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> P </div>", transfer: 1500}],
['pto_b_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', q: "&nbsp;&nbsp;&nbsp;&nbsp;click on the sound", hasCorrect: false, timeout: 4000, hideProgressBar: true}, "Separator", 125, "Message", {html: "<div style='text-align: center;'> answer: </div><div style='text-align: center; font-size: 150%; margin-top: 12pt;'> T </div>", transfer: 1500}],


// test trials
['t_b_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_5', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_05.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_05.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_6', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_06.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_06.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_7', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_07.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_07.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_8', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_08.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_08.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_9', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_09.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_09.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],

['t_sp_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_5', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_05.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_05.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_6', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_06.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_06.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_7', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_07.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_07.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_8', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_08.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_08.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_9', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_09.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_09.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],

['t_b_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_5', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_05.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_05.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_6', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_06.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_06.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_7', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_07.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_07.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_8', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_08.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_08.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_9', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_09.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_09.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_b_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/p_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],

['t_sp_1', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_01.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_5', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_05.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_05.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_6', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_06.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_06.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_7', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_07.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_07.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_8', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_08.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_08.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_9', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_09.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_09.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],
['t_sp_18', "Trial", {html: '<audio autoplay="autoplay"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.wav" type="audio/wav"> <source src="https://udrive.oit.umass.edu/kmullin/_exp/sp_uh_iu_01_pt_18.mp3" type="audio/mpeg"> Your browser does not support the audio element.</audio>', hideProgressBar: true}],




[["itdoesntmatchanything", 124], "Trial", {s: ""}] 
// Be sure there's no comma after the LAST ITEM !!

];

