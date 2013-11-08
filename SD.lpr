program SD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  strutils, math, IniFiles;

const
  StartDir: integer = 1236;
  EndDir: integer = 1399;

  DebugMode: boolean = true;//false;

type
  TSolution = (solIgs, solEu, solMa1);
  TCoordinate = (crdX, crdY, crdZ);
  TFloatList = array of Extended;

const
  SectionName: array [solIgs..solMa1] of string = ('Igs', 'Eu', 'Ma1');

type
  { TWeekFiles }

  TWeekFiles = class
  private
    FMask: string;
    FStart, FEnd: integer;
    FSection: string;
  public
    constructor Create(aIni: TIniFile; aIndex: integer; const aSection: string);
    property Mask: string read FMask write FMask;
    property WeekStart: integer read FStart write FStart;
    property WeekEnd: integer read FEnd write FEnd;
    property Section: string read FSection write FSection;
  end;

  { TSolutionFile }

  TSolutionFile = class
  private
    FWeek: TStringList;
    FSection: string;
  public
    constructor Create(aIni: TIniFile; const aSection: string);
    destructor Destroy; override;
    function GetFilename(aWeekNmb: integer): string;
    property Section: string read FSection write FSection;
  end;

  { TSD }
  TSD = class(TCustomApplication)
  private
    ExePath: string;
    f: Text;
    CommonStationList: TStringList;
    CoordList: array [0..2] of TFloatList;
    CommonList: TFloatList;

    procedure PrepareCommonLists(aCoord: TCoordinate; aWeekNumber: integer);
    procedure ProceedPair(aCoord: TCoordinate; aSolution1, aSolution2: TSolution;
      const aWeekNumber: integer; var aDval, aSval: Extended);

    function GetCommonStationList(aWeekNumber: integer): TStringList;

    function GetStationListForSolution(aSolution: TSolution;
             aWeekNumber: integer): TStringList;

    function GetCoordinateListForSolution(aSolution: TSolution;
             aWeekNumber: integer; aCoordinate: TCoordinate): TFloatList;
    function CenterValues(var aCoordinateList: TFloatList;
      aCommonList: TFloatList): boolean;
    procedure DebugList(const aCaption: string; aCoordinateList: TFloatList); overload;
    procedure DebugList(const aCaption: string; aCoordinateList: TStringList); overload;

    function GetFileNameForSolution(aSolution: TSolution; aWeekNumber: integer): string;

    // input: 17  ANDO 10333M001     2175765.9035    624248.2065   5943417.7863    I
    // output: ANDO
    function GetStation(const aString: string): string;

    // input: 17  ANDO 10333M001     2175765.9035    624248.2065   5943417.7863    I
    // output: 2175765.9035 for crdX, .., 5943417.7863 for crdZ
    function GetCoordinate(const aString: string; aCoordinate: TCoordinate): Extended;

    function GetToken(aString: string; const SepChar: string; TokenNum: integer): string;
    function RemoveDupSpaces(const aString: string): string;
    function GetDefDir(aSolution: TSolution): string;
    procedure Initialize;
  protected
    Dirs: array [solIgs..solMa1] of string;
    SolutionFiles: array [solIgs..solMa1] of TSolutionFile;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

  end;

{ TSolutionFile }

constructor TSolutionFile.Create(aIni: TIniFile; const aSection: string);
var
  i, n: integer;
  WeekFile: TWeekFiles;
begin
  FWeek := TStringList.Create;
  Section := aSection;
  if Assigned(aIni) then
    n := aIni.ReadInteger(Section, 'Count', 0)
  else
    n := 1;

  for i := 1 to n do
  begin
    WeekFile := TWeekFiles.Create(aIni, i, Section);
    FWeek.AddObject(WeekFile.Mask, WeekFile);
  end;
end;

destructor TSolutionFile.Destroy;
var
  i: integer;
begin
  for i := FWeek.Count - 1 downto 0 do
    FWeek.Objects[i].Free;
  FWeek.Free;
  inherited Destroy;
end;

function TSolutionFile.GetFilename(aWeekNmb: integer): string;
var
  i: integer;
  WeekFile: TWeekFiles;
begin
  for i := 0 to FWeek.Count - 1 do
  begin
    WeekFile := TWeekFiles(FWeek.Objects[i]);
    if (aWeekNmb >= WeekFile.WeekStart) and (aWeekNmb <= WeekFile.WeekEnd) then
    begin
      Result := AnsiReplaceText(WeekFile.Mask, 'XXXX', Format('%4.4d', [aWeekNmb]));
      exit;
    end;
  end;

  Result := '';
end;

{ TWeekFiles }

constructor TWeekFiles.Create(aIni: TIniFile; aIndex: integer; const aSection: string);
begin
  inherited Create;

  Section := aSection;
  if Assigned(aIni) then
  begin
    Mask := aIni.ReadString(Section, 'Filename' + IntToStr(aIndex), '');
    WeekStart := aIni.ReadInteger(Section, 'Filename' + IntToStr(aIndex) + 'start', 0);
    WeekEnd := aIni.ReadInteger(Section, 'Filename' + IntToStr(aIndex) + 'end', 9999);
  end
  else
  begin
    Mask := '';
    WeekStart := 0;
    WeekEnd := 9999;
  end;
end;

{ TSD }

procedure TSD.Initialize;
var
  i: TSolution;
  Ini: TIniFile;
begin
  if FileExists(ExePath + 'sd.ini') then
  begin
    Ini := TIniFile.Create(ExePath + 'sd.ini');
    try
      DebugMode := Ini.ReadBool('Common', 'DebugMode', false);
      StartDir := Ini.ReadInteger('Common', 'StartWeek', 1236);
      EndDir := Ini.ReadInteger('Common', 'EndWeek', 1399);

      for i := solIgs to solMa1 do
      begin
        SolutionFiles[i] := TSolutionFile.Create(Ini, SectionName[i]);
        Dirs[i] := Ini.ReadString('Directories', SectionName[i], GetDefDir(i));
        if Dirs[i][2] <> ':' then
          Dirs[i] := ExePath + Dirs[i];
      end;
    finally
      Ini.Free;
    end;
  end
  else
    for i := solIgs to solMa1 do
      Dirs[i] := GetDefDir(i);
end;

procedure TSD.ProceedPair(aCoord: TCoordinate; aSolution1, aSolution2: TSolution;
  const aWeekNumber: integer; var aDval, aSval: Extended);
var
  i, j: integer;
  StationsCount: integer;
  SolutionNumber: TSolution;
  LinesList: TStringList;

  s: string;
  CurrentStation: string;
  List1, List2: TFloatList;
begin
  aDval := 0;
  aSval := 0;

  List1 := CoordList[Ord(aSolution1)];
  List2 := CoordList[Ord(aSolution2)];

  if Length(List1) <> Length(List2) then
  begin
    WriteLn('Wrong list dimension!');
    exit;
  end;

  for i := Low(CommonList) to High(CommonList) do
    CommonList[i] := Sqr(List1[i] - List2[i]);
  DebugList('List for difference:', CommonList);
  if Length(CommonList) > 0 then
    aDval := Mean(CommonList)
  else
    aDval := 0;
  WriteLn(f, '        d = ', FloatToStr(aDval));

  for i := Low(CommonList) to High(CommonList) do
    CommonList[i] := Sqr(List1[i] + List2[i]);
  DebugList('List for summary:', CommonList);
  if Length(CommonList) > 0 then
    aSval := Mean(CommonList)
  else
    aSval := 0;
  WriteLn(f, '        s = ', FloatToStr(aSval));
end;

procedure TSD.PrepareCommonLists(aCoord: TCoordinate; aWeekNumber: integer);
var
  i, j: integer;
begin
  for i := 0 to 2 do
    CoordList[i] := GetCoordinateListForSolution(TSolution(i), aWeekNumber, aCoord);

  SetLength(CommonList, Length(CoordList[0]));
  for i := Low(CommonList) to High(CommonList) do
  begin
    CommonList[i] := 0;
    for j := 0 to 2 do
      CommonList[i] := CommonList[i] + CoordList[j][i];
    CommonList[i] := CommonList[i] / 3;
  end;

  for i := 0 to 2 do
    if not CenterValues(CoordList[i], CommonList) then
      WriteLn('Problems in week #' + IntToStr(aWeekNumber));
    ;
end;

function TSD.GetToken(aString: string; const SepChar: string; TokenNum: integer): string;
var
  Token: string;
  StrLen: integer;
  TNum: integer;
  TEnd: integer;
begin
  aString := Trim(aString);
  StrLen := Length(aString);
  TNum := 1;
  TEnd := StrLen;

  while ((TNum <= TokenNum) and (TEnd <> 0)) do
  begin
    TEnd := Pos(SepChar, aString);
    if TEnd <> 0 then
    begin
      Token := Copy(aString, 1, TEnd - 1);
      Delete(aString, 1, TEnd);
      Inc(TNum);
    end
    else
      Token := aString;
  end;

  if TNum >= TokenNum then
    Result := Token
  else
    Result := '';

  Result := Trim(Result);
end;


function TSD.GetCommonStationList(aWeekNumber: integer): TStringList;
var
  i: integer;
  StationList: array [0..2] of TStringList;
begin
  Result := TStringList.Create;

  for i := 0 to 2 do
  begin
    StationList[i] := GetStationListForSolution(TSolution(i), aWeekNumber);
    WriteLn(f, 'Solution ' + IntToStr(i) + ': ' + StationList[i].DelimitedText);
  end;

  try
    for i := 0 to StationList[0].Count - 1 do
      if (StationList[1].IndexOf(StationList[0][i]) > -1) and
         (StationList[2].IndexOf(StationList[0][i]) > -1) and
         (Result.IndexOf(StationList[0][i]) = -1) then
        Result.Add(StationList[0][i]);

    for i := 0 to StationList[1].Count - 1 do
      if (StationList[0].IndexOf(StationList[1][i]) > -1) and
         (StationList[2].IndexOf(StationList[1][i]) > -1) and
         (Result.IndexOf(StationList[1][i]) = -1) then
        Result.Add(StationList[1][i]);

    for i := 0 to StationList[2].Count - 1 do
      if (StationList[1].IndexOf(StationList[2][i]) > -1) and
         (StationList[0].IndexOf(StationList[2][i]) > -1) and
         (Result.IndexOf(StationList[2][i]) = -1) then
        Result.Add(StationList[2][i]);

    Result.Sort;
    DebugList('Common list station: ', Result);
  finally
    for i := 0 to 2 do
      StationList[i].Free;
  end;
end;

function TSD.GetStationListForSolution(aSolution: TSolution;
  aWeekNumber: integer): TStringList;
var
  FileName: string;
  i: integer;
begin
  Result := TStringList.Create;
  FileName := GetFileNameForSolution(aSolution, aWeekNumber);
  if not FileExists(FileName) then
  begin
    WriteLn('File ' + FileName + ' not found.');
    Exit;
  end;

  Result.LoadFromFile(FileName);
  for i := 0 to 5 do
    Result.Delete(0);
  for i := 0 to Result.Count - 1 do
    Result[i] := GetStation(Result[i]);

  for i := Result.Count - 1 downto 0 do
    if Trim(Result[i]) = '' then
      Result.Delete(i);
  Result.Sort;
end;

function TSD.GetCoordinateListForSolution(aSolution: TSolution;
  aWeekNumber: integer; aCoordinate: TCoordinate): TFloatList;
var
  FileName, Station: string;
  i: integer;
  sl: TStringList;
  Coord: Extended;
begin
  SetLength(Result, 0);
  FileName := GetFileNameForSolution(aSolution, aWeekNumber);
  if not FileExists(FileName) then
    Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    for i := 0 to 5 do
      sl.Delete(0);
    for i := 0 to sl.Count - 1 do
    begin
      if Trim(sl[i]) = '' then
        Continue;
      Station := GetStation(sl[i]);
      if Trim(Station) = '' then
        Continue;
      if CommonStationList.IndexOf(Station) = -1 then
        Continue;

      Coord := GetCoordinate(sl[i], aCoordinate);
      if Coord <> 999999999 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Coord;
      end;
    end;
  finally
    sl.Free;
  end;

  DebugList('List for solution: ' + IntToStr(Ord(aSolution)) +
    ', WeekNumber: ' + IntToStr(Ord(aWeekNumber)) + ', Coordinate: ' +
    IntToStr(Ord(aCoordinate)), Result);
end;

function TSD.CenterValues(var aCoordinateList: TFloatList;
  aCommonList: TFloatList): boolean;
var
  i: integer;
begin
  Result := false;
  if Length(aCoordinateList) = 0 then
  begin
    WriteLn('Empty list!');
    Exit;
  end;

  if Length(aCoordinateList) <> Length(aCommonList) then
  begin
    WriteLn('Different list dimension!');
    Exit;
  end;

  for i := Low(aCoordinateList) to High(aCoordinateList) do
    aCoordinateList[i] := aCoordinateList[i] - aCommonList[i];

  DebugList('Centered list:', aCoordinateList);
  Result := true;
end;

procedure TSD.DebugList(const aCaption: string; aCoordinateList: TFloatList);
var
  i: integer;
begin
  if not DebugMode then
    exit;

  WriteLn(f, aCaption);
  for i := Low(aCoordinateList) to High(aCoordinateList) do
    WriteLn(f, '            ' + FloatToStr(aCoordinateList[i]));
  WriteLn(f, ' ');
end;

procedure TSD.DebugList(const aCaption: string; aCoordinateList: TStringList);
var
  i: integer;
begin
  if not DebugMode then
    exit;

  WriteLn(f, aCaption);
  for i := 0 to aCoordinateList.Count - 1 do
    WriteLn(f,  '            ' + aCoordinateList[i]);
  WriteLn(f, ' ');
end;


function TSD.GetFileNameForSolution(aSolution: TSolution; aWeekNumber: integer): string;
begin
  Result := Dirs[aSolution] + SolutionFiles[aSolution].GetFilename(aWeekNumber);
end;

function TSD.GetStation(const aString: string): string;
begin
  Result := RemoveDupSpaces(aString);
  Result := GetToken(Result, ' ', 2);
end;

function TSD.GetCoordinate(const aString: string; aCoordinate: TCoordinate): Extended;
var
  s: string;
begin
  s := RemoveDupSpaces(aString);
  //s := GetToken(s, ' ', 4 + Ord(aCoordinate));

  case aCoordinate of
    crdX: s := GetToken(s, ' ', 4);
    crdY: s := GetToken(s, ' ', 5);
  else
    s := GetToken(s, ' ', 6);
  end;
  s := AnsiReplaceStr(s, '.', ',');

  Result := StrToFloatDef(s, 999999999);
  Result := (Result);
end;



function TSD.RemoveDupSpaces(const aString: string): string;
var
  i: integer;
begin
  Result := aString;

  for i := 0 to 5 do
    Result := AnsiReplaceStr(Result, '  ', ' ');

  Result := Trim(Result);
end;

function TSD.GetDefDir(aSolution: TSolution): string;
begin
  case aSolution of
    solIgs:
      Result := ExePath + 'igs_crd_hlm\igs\crd\';
    solEu:
      Result := ExePath + 'igs_crd_hlm\igs_eu\crd\';
    solMa1:
      Result := ExePath + 'igs_crd_hlm\igs_ma1\crd\';
  else
    Result := '';
  end;
end;

procedure TSD.DoRun;
const
  FileHeader = 'W/N    s1 (IGS-EU0) s2 (EU0_MA1) s3 (IGS-MA1)  ro12       ro13        ro23';
var
  ErrorMsg: String;
  WeekNumber: integer;
  s12, s13, s23, d12, d13, d23: Extended;
  sigma1, sigma2, sigma3: Extended;
  ro12, ro13, ro23: Extended;
  ResultList, ResultListMM: TStringList;
  Coord: TCoordinate;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

try
  { add your program here }
  if not DirectoryExists(ExePath + 'output\') then
    CreateDir(ExePath + 'output\');

  AssignFile(f, ExePath + 'output\debug.out');
  Rewrite(f);
  ResultList := TStringList.Create;
  ResultListMM := TStringList.Create;
  try
    for Coord := crdX to crdZ do
    begin
      WriteLn(f, 'Coord = ' + IntToStr(Ord(Coord)));
      ResultList.Clear;
      ResultList.Add(FileHeader);

      ResultListMM.Clear;
      ResultListMM.Add(FileHeader);

      for WeekNumber := StartDir to EndDir do
      begin
        WriteLn(f, '  WeekNumber = ' + IntToStr(WeekNumber));

        CommonStationList := GetCommonStationList(WeekNumber);
        try
          PrepareCommonLists(Coord, WeekNumber);

          ProceedPair(Coord, solIgs, solEu, WeekNumber, d12, s12);
          ProceedPair(Coord, solIgs, solMa1, WeekNumber, d13, s13);
          ProceedPair(Coord, solEu, solMa1, WeekNumber, d23, s23);

          sigma1 := Sqrt((s12 + s13 - s23 + d12 + d13 - d23) / 4);
          sigma2 := Sqrt((s12 + s23 - s13 + d12 + d23 - d13) / 4);
          sigma3 := Sqrt((s23 + s13 - s12 + d23 + d13 - d12) / 4);

          if (sigma1 > 0) and (sigma2 > 0) and (sigma3 > 0) then
          begin
            ro12 := (s12 - d12) / 4 / sigma1 / sigma2;
            ro13 := (s13 - d13) / 4 / sigma1 / sigma3;
            ro23 := (s23 - d23) / 4 / sigma2 / sigma3;

            WriteLn(f, '    sigma1 = ' + FloatToStr(sigma1));
            WriteLn(f, '    sigma2 = ' + FloatToStr(sigma2));
            WriteLn(f, '    sigma3 = ' + FloatToStr(sigma3));

            WriteLn(f, '    ro12 = ' + FloatToStr(ro12));
            WriteLn(f, '    ro13 = ' + FloatToStr(ro13));
            WriteLn(f, '    ro23 = ' + FloatToStr(ro23));

            ResultList.Add(Format('%d    %8.6f    %8.6f    %8.6f    %8.6f    %8.6f    %8.6f',
              [WeekNumber, sigma1, sigma2, sigma3, ro12, ro13, ro23]));

            ResultListMM.Add(Format('%d    %8.6f    %8.6f    %8.6f    %8.6f    %8.6f    %8.6f',
              [WeekNumber, sigma1 * 1000, sigma2 * 1000, sigma3 * 1000, ro12, ro13, ro23]));
          end;
        finally
          CommonStationList.Free;
        end;
      end;

      ResultList.SaveToFile(ExePath + 'output\output' + IntToStr(Ord(Coord)) + '.dat');
      ResultListMM.SaveToFile(ExePath + 'output\output' + IntToStr(Ord(Coord)) + '_mm.dat');
    end;
  finally
    ResultList.Free;
    ResultListMM.Free;
    CloseFile(f);
  end;
except
  ShowException(Exception.Create(ErrorMsg));
  Terminate;
  Exit;
end;

  WriteLn('Done.');
  ReadLn;

  // stop program loop
  Terminate;
end;

constructor TSD.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;

  ExePath := IncludeTrailingPathDelimiter(ExtractFilePath(ExeName));

  Initialize;
end;

destructor TSD.Destroy;
begin
  inherited Destroy;
end;

procedure TSD.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TSD;

{$R *.res}

begin
  Application:=TSD.Create(nil);
  Application.Run;
  Application.Free;
end.

