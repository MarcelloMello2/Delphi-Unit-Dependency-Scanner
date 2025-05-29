unit duds.analyzer.Stats;

interface

type
  TUnitStats = class
  public

    ParsedFileCount  : Integer;
    FilesNotInPath   : Integer;
    ScannedUsesCount : Integer;
    DeepestScanDepth : Integer;
    LinesOfCode      : Integer;

    FMXFormCount     : Integer;
    VCLFormCount     : Integer;

    CircularFiles    : Integer;
    SemiCircularFiles: Integer;

    constructor Create;

    procedure Clear;

  end;

  TAnalyzerStats = class
  public
    Units: TUnitStats;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

  end;

implementation

{ TUnitStats }

procedure TUnitStats.Clear;
begin
  FMXFormCount      := 0;
  VCLFormCount      := 0;
  CircularFiles     := 0;
  SemiCircularFiles := 0;
  LinesOfCode       := 0;
  ParsedFileCount   := 0;
  FilesNotInPath    := 0;
  ScannedUsesCount  := 0;
  DeepestScanDepth  := 0;
end;

constructor TUnitStats.Create;
begin
  inherited Create;
  Clear;
end;

{ TAnalyzerStats }

procedure TAnalyzerStats.Clear;
begin
  Units.Clear;
end;

constructor TAnalyzerStats.Create;
begin
  inherited Create;
  Units := TUnitStats.Create;
end;

destructor TAnalyzerStats.Destroy;
begin
  Units.Free;
  inherited Destroy;
end;

end.
