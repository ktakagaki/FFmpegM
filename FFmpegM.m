(* ::Package:: *)

(* Mathematica Package *)

(* Created by the Wolfram Workbench 30.01.2013 *)


BeginPackage["FFmpegM`"]


(*Modify the following if you want a specific temporary directory*)
$FFmpegTempDirDefault="";


ImportFFmpeg::usage="Uses FFmpeg backend to import videos.";
ExportFFmpeg::usage="Uses FFmpeg backend to export videos.";


(*Set FFmpegTempDir, input value or default values*)
If[  Head[FFmpegTempDir]=!="String",
	If[$FFmpegTempDirDefault==="",
		FFmpegTempDir=$UserBaseDirectory<>"/ApplicationData/FFmpgTmp",
		FFmpegTempDir=$FFmpegTempDirDefault 	]
];
If[ FileNames[FFmpegTempDir]=={}, CreateDirectory[FFmpegTempDir]];
If[ FileNames[FFmpegTempDir<>"/*"]!={}, DeleteFile[FileNames[FFmpegTempDir<>"/*"]]];


FFmpegTempDir::usage="Specify where to keep temporary files.";
(*FFprobeVersion::usage="FFprobe version number.";*)


Options[ImportFFmpeg]={FFmpegCommandlineOptions->""};


Begin["`Private`"]


ImportFFmpeg[fileName_String, opts:OptionsPattern[]]:=
	Block[{dir,files,ret},

		dir=Directory[]; SetDirectory[FFmpegTempDir];
		files=FileNames["t*.png"]; DeleteFile[files];

		Run["ffmpeg -i " <> fileName <> " -f image2 "<>
				OptionValue[FFmpegCommandlineOptions]<> " " <>"t%07d.png"];

		files=FileNames["t*.png"];
		ret=Import /@ files;
		DeleteFile[files];
		SetDirectory[dir];

		ret
	];


ImportFFmpeg[fileName_String, {start_, end_}, opts:OptionsPattern[]]:=
Block[{dir, tempLog, fps, files, ret},
	dir=Directory[]; SetDirectory[FFmpegTempDir];
	files=FileNames["t*.png"]; DeleteFile[files];
	files=FileNames["t*.png"]; 
    
    (*Use ffprobe utility to read video file information*)
    Run["ffprobe -show_streams "<>fileName <>" > tempFFmpeg.log"];
	tempLog=Import["tempFFmpeg.log", "String"];

    fps = ToExpression[
          StringReplace[tempLog, 
              (___ ~~ "r_frame_rate=" ~~ x:Except[WhitespaceCharacter]..~~{EndOfLine | WhitespaceCharacter} ~~ ___->x)
          ]];
	DeleteFile["tempFFmpeg.log"];

    (*actually import the video file*)
	ret=ImportFFmpeg[fileName, 
		Sequence@@Prepend[{opts},
			FFmpegCommandlineOptions->"-ss "<>ToString[N[start/fps]]<>" -vframes " <>ToString[end-start+1]<>" "
				<>OptionValue[FFmpegCommandlineOptions]]
	];

	SetDirectory[dir];
	ret

];


ImportFFmpeg[args___]:=Message[ImportFFmpeg::invalidArgs,{args}];
ImportFFmpeg::invalidArgs="Arguments `1` are invalid!";


ExportFFmpeg[fileName_String, manipulate_Manipulate]:=
	Block[{dir,files},
		dir=Directory[];
		SetDirectory[FFmpegTempDir];
		
		Export["t0000001.png", manipulate,"VideoFrames"];
		Run["ffmpeg -f image2 -i " <> "t%07d.png -c:v libx264 -preset medium " <> fileName <> " "];
		files=FileNames["t*.png"]; DeleteFile[files];
		SetDirectory[dir];
		fileName
	];


ExportFFmpeg[fileName_String, images_List]:=
	Block[{dir,files},
		dir=Directory[];
		SetDirectory[FFmpegTempDir];
		
		Do[
			Export["t"<>Apply[StringJoin,Map[ToString,IntegerDigits[cnt, 10, 7] ]]<>".png",
				 images[[cnt]] 
			],
		{cnt,1,Length[images]}];
		Run["ffmpeg -f image2 -i " <> "t%07d.png -c:v libx264 -preset medium " <> fileName <> " "];
		files=FileNames["t*.png"]; DeleteFile[files];
		SetDirectory[dir];
		fileName
	];


ExportFFmpeg[args___]:=Message[ExportFFmpeg::invalidArgs,{args}];
ExportFFmpeg::invalidArgs="Arguments `1` are invalid!";


End[]

EndPackage[]


(* ::Section::Closed:: *)
(*Backup*)


(*
	Switch[FFprobeVersion,
     "S", Run["ffprobe -show_streams "<>fileName <>" > tempFFmpeg.log"],
	  _ , If[FFprobeVersion \[NotEqual] "1", Message[ImportFFmpeg::ffprobeversion]];
		  Run["ffprobe -report -i "<>fileName <>" > tempFFmpeg.log"]
      ];*)


(*	fps=ToExpression[StringReplace[
		StringCases[tempLog, {NumberString ~~ " fps" , NumberString ~~ " tbr"}][[1]] ,
		(x:NumberString~~" "~~__) :> x
	]];*)


(*Run["ffprobe -version > tempFFmpeg.log"];
FFprobeVersion = StringTake[StringReplace[
	Import["tempFFmpeg.log"],
	(___ ~~ "version "~~x:Except[WhitespaceCharacter].. ~~ ___  ~~ WhitespaceCharacter\[Rule] x)  
	], 1];*)


(*ImportFFmpeg::ffprobeversion="ffprobe version was not recognized based on parse `1`. Will continue with version 1 syntax";*)
