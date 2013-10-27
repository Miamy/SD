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
  StartDir = 1236;
  EndDir = 1399;

  DebugMode = true;//false;

type
  TSolution = (solIgs, solEu, solMa1);
  TCoordinate = (crdX, crdY, crdZ);
  TFloatList = array of Extended;

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
    procedure CenterValues(var aCoordinateList: TFloatList;
      aCommonList: TFloatList);
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
  protected
    Dirs: array [solIgs..solMa1] of string;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

  end;

{ TSD }

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
  aDval := Mean(CommonList);
  WriteLn(f, '        d = ', FloatToStr(aDval));

  for i := Low(CommonList) to High(CommonList) do
    CommonList[i] := Sqr(List1[i] + List2[i]);
  DebugList('List for summary:', CommonList);
  aSval := Mean(CommonList);
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
    CenterValues(CoordList[i], CommonList);
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
    StationList[i] := GetStationListForSolution(TSolution(i), aWeekNumber);

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
    Exit;

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

procedure TSD.CenterValues(var aCoordinateList: TFloatList;
  aCommonList: TFloatList);
var
  i: integer;
begin
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


function TSD.GetFileNameForSolution(aSolution: TSolution; aWeekNumber: integer
  ): string;
begin
  case aSolution of
    solIgs:
      begin
        if aWeekNumber < 1252 then
          Result := Dirs[aSolution] + Format('igb03P%4.0d.CRD', [aWeekNumber])
        else if aWeekNumber < 1304 then
          Result := Dirs[aSolution] + Format('igb04P%4.0d.CRD', [aWeekNumber])
        else if aWeekNumber < 1356 then
          Result := Dirs[aSolution] + Format('igb05P%4.0d.CRD', [aWeekNumber])
        else
          Result := Dirs[aSolution] + Format('igb06P%4.0d.CRD', [aWeekNumber]);
      end;
    solEu:  Result := Dirs[aSolution] + Format('%4.0d7_i_eu.CRD', [aWeekNumber]);
    solMa1: Result := Dirs[aSolution] + Format('%4.0d7_i_ma1.CRD', [aWeekNumber]);
    else
      Result := '';
  end;
end;

function TSD.GetStation(const aString: string): string;
begin
  Result := RemoveDupSpaces(aString);
  Result := GetToken(Result, ' ', 2);
end;

function TSD.GetCoordinate(const aString: string; aCoordinate: TCoordinate
  ): Extended;
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
  Result := {Abs}(Result);
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
  AssignFile(f, ExePath + 'debug.out');
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
        finally
          CommonStationList.Free;
        end;
      end;

      ResultList.SaveToFile(ExePath + 'output' + IntToStr(Ord(Coord)) + '.dat');
      ResultListMM.SaveToFile(ExePath + 'output' + IntToStr(Ord(Coord)) + '_mm.dat');
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
var
   Ini: TIniFile;
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  ExePath := ExtractFilePath(ExeName);

  if FileExists(ExePath + 'sd.ini') then
  begin
    Ini := TIniFile.Create(ExePath + 'sd.ini');
    try
      Dirs[solIgs] := Ini.ReadString('Directories', 'DirIgs', ExePath + 'igs_crd_hlm\igs\crd\');
      Dirs[solEu]  := Ini.ReadString('Directories', 'DirEu', ExePath + 'igs_crd_hlm\igs_eu\crd\');
      Dirs[solMa1] := Ini.ReadString('Directories', 'DirMa1', ExePath + 'igs_crd_hlm\igs_ma1\crd\');
    finally
      Ini.Free;
    end;
  end
  else
  begin
    Dirs[solIgs] := ExePath + 'igs_crd_hlm\igs\crd\';
    Dirs[solEu]  := ExePath + 'igs_crd_hlm\igs_eu\crd\';
    Dirs[solMa1] := ExePath + 'igs_crd_hlm\igs_ma1\crd\';
  end;
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

