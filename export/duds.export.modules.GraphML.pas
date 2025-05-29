unit duds.export.modules.GraphML;

interface

uses
  System.SysUtils,

  Xml.XMLDoc, Xml.XMLIntf,

  duds.common.Utils,
  duds.common.Types,
  duds.common.Strings,
  duds.common.Classes,
  duds.common.Files,
  duds.common.Language,
  duds.common.Log,
  duds.common.Interfaces,
  duds.analyzer.model;

type
  TExportModulesToGraphML = class
  private
    FModel: TDudsModel;
    fProjectSettings: TProjectSettings;
    FOnLog: TLogProcedure;
    fFileName: string;
    fOriginsToExport: TModuleOrigins;

    procedure AddEdge(xmlGraph: IXMLNode; edgeID: Integer; CurrentModule,
        DependendModule: TModule; aUsedModuleData: TUsedModuleData);
    procedure AddEdgeLabel(edge: IXMLNode; DependencyViolation: Boolean;
        NumerOfUnits: Integer; position: string);
    procedure AppendModuleNodes(xmlGraph: IXMLNode);
    procedure AppendDependencies(xmlGraph: IXMLNode);

    function ModuleIsPartOfExport(Module: TModule): Boolean;

  public
    constructor Create;

    procedure ExportModules;

    property Model: TDudsModel                 read FModel           write FModel;
    property OnLog: TLogProcedure              read FOnLog           write FOnLog;
    property ProjectSettings: TProjectSettings read fProjectSettings write fProjectSettings;
    property FileName: string                  read fFileName        write fFileName;

    property OriginsToExport: TModuleOrigins   read fOriginsToExport write fOriginsToExport;
  end;


implementation

{ TExportModulesToGraphML }

constructor TExportModulesToGraphML.Create;
begin
  fOriginsToExport := [moUndefined, moDelphi, mo3rdParty, moOwn];
end;

function TExportModulesToGraphML.ModuleIsPartOfExport(Module: TModule): Boolean;
begin
  Result := Module.Origin in fOriginsToExport;
end;

const
  cNodeKey     = 'd0';
  cEdgeKey     = 'd1';
  cEdgeHintKey = 'd2';

const
                                                              {moUndefined,  moDelphi, mo3rdParty,  moOwn}
  cModuleOriginLightColors   : array[TModuleOrigin] of string = ('#CCCCCC', '#FFB565', '#A6D8E7', '#AADB1E');
  cModuleOriginMediumColors  : array[TModuleOrigin] of string = ('#CCCCCC', '#FF8400', '#49AFD9', '#60B515');
  cModuleOriginDarkColors    : array[TModuleOrigin] of string = ('#313131', '#AA4500', '#003D79', '#1D5100');

  cDependencyAllowedColor     = '#808080';
  cDependencyViolationColor   = '#FF0000';

  cBackgroundColor = '#FFFFFF';

procedure TExportModulesToGraphML.ExportModules;
var
  Xml: IXMLDocument;
  EdgeID, NodeID: Integer;
  graphml, Key, graph: IXMLNode;
begin
  // Internally GraphML is an XML document
  Xml                             := TXMLDocument.Create(nil);
  Xml.Active                      := TRUE;
  Xml.Version                     := '1.0';
  Xml.Encoding                    := 'UTF-8';
  Xml.StandAlone                  := 'no';
  Xml.Options                     := [doNodeAutoIndent];

  graphml                         := Xml.AddChild('graphml');
  graphml.Attributes['xmlns:y']   := 'http://www.yworks.com/xml/graphml';
  graphml.Attributes['xmlns:yed'] := 'http://www.yworks.com/xml/yed/3';

  Key                             := graphml.AddChild('key');
  Key.Attributes['id']            := cNodeKey;
  Key.Attributes['for']           := 'node';
  Key.Attributes['yfiles.type']   := 'nodegraphics';

  Key                             := graphml.AddChild('key');
  Key.Attributes['id']            := cEdgeKey;
  Key.Attributes['for']           := 'edge';
  Key.Attributes['yfiles.type']   := 'edgegraphics';

  Key                             := graphml.AddChild('key');
  Key.Attributes['id']            := cEdgeHintKey;
  Key.Attributes['for']           := 'edge';
  Key.Attributes['attr.name']     := 'description';
  Key.Attributes['attr.type']     := 'string';

  graph                           := graphml.AddChild('graph');
  graph.Attributes['edgedefault'] := 'directed';

  AppendModuleNodes(graph);
  AppendDependencies(graph);

  Xml.SaveToFile(Filename);
  Xml.Active := FALSE;
end;

procedure TExportModulesToGraphML.AppendModuleNodes(xmlGraph: IXMLNode);

  function BuildNodeHtmlText(module: TModule): string;
  const
    cHtmlTemplate = '<html>'         +
                    '  <div>'        +
                    '  <b>%s </b>'   + // main label in bold
                    '%s'             + // <- insert additional lines here
                    '  </div>'       +
                    '</html>';
  var
    AdditionalLines: string;
  begin
    AdditionalLines := '';

    if module.AnalysisData.NumberOfFiles > 0 then
      AddToken(AdditionalLines, Format('<br></br>Files: %s', [FormatCardinal(module.AnalysisData.NumberOfFiles)]), '');

    if module.AnalysisData.NumberOfFilesNotInPath > 0 then
      AddToken(AdditionalLines, Format('<br></br><font style="color:red">Files not in Path: %s</font>', [FormatCardinal(module.AnalysisData.NumberOfFilesNotInPath)]), '');

    if module.AnalysisData.LinesOfCode > 0 then
      AddToken(AdditionalLines, Format('<br></br>LoC: %s', [FormatCardinal(module.AnalysisData.LinesOfCode)]), '');

    Result := Format(cHtmlTemplate, [module.Name, AdditionalLines]);
  end;

var
  CurrentModule: TModule;
  ModuleNode: IXMLNode;
  ModuleData: IXMLNode;
  ShapeNode: IXMLNode;
  FillNode: IXMLNode;
  BorderStyleNode: IXMLNode;
  GeometryNode: IXMLNode;
begin
  for CurrentModule in FModel.Modules.OrderedModules do
    if ModuleIsPartOfExport(CurrentModule) then
    begin
      ModuleNode := xmlGraph.AddChild('node');
      ModuleNode.Attributes['id'] := 'n' + IntToStr(CurrentModule.ID);

      ModuleData := ModuleNode.AddChild('data');
      ModuleData.Attributes['key'] := cNodeKey;

      ShapeNode := ModuleData.AddChild('y:ShapeNode');

      ShapeNode.AddChild('y:NodeLabel').Text := BuildNodeHtmlText(CurrentModule);

      if CurrentModule.Usage = muTest then
        ShapeNode.AddChild('y:Fill').Attributes['color']        := cModuleOriginLightColors[CurrentModule.Origin]
      else
        ShapeNode.AddChild('y:Fill').Attributes['color']        := cModuleOriginMediumColors[CurrentModule.Origin];

      BorderStyleNode := ShapeNode.AddChild('y:BorderStyle');
      BorderStyleNode.Attributes['color'] := cModuleOriginDarkColors[CurrentModule.Origin];
      // border style shows production vs. test code
      if CurrentModule.Usage = muTest then
         BorderStyleNode.Attributes['type'] := 'dashed';

      GeometryNode := ShapeNode.AddChild('y:Geometry');
      GeometryNode.Attributes['width'] := '150';

    end;
end;

procedure TExportModulesToGraphML.AddEdgeLabel(edge: IXMLNode;
  DependencyViolation: Boolean; NumerOfUnits: Integer; position: string);
var
  edgeLabelOutgoing: IXMLNode;
begin
  // label for the edge with number of outgoing units
  edgeLabelOutgoing                                        := edge.AddChild('y:EdgeLabel');
  edgeLabelOutgoing.Attributes['modelName']                := 'three_center';     // put the label over the edge
  edgeLabelOutgoing.Attributes['modelPosition']            := position;           // position at the edge "source" or "target"
  edgeLabelOutgoing.Attributes['backgroundColor']          := cBackgroundColor;   // background color for better readability of the edge-label

  edgeLabelOutgoing.Text := FormatCardinal(NumerOfUnits);

  if DependencyViolation then
  begin
    edgeLabelOutgoing.Attributes['backgroundColor'] := cDependencyViolationColor;
    edgeLabelOutgoing.Attributes['textColor']       := cBackgroundColor;
  end;
end;

procedure TExportModulesToGraphML.AddEdge(xmlGraph: IXMLNode; edgeID: Integer; CurrentModule, DependendModule: TModule; aUsedModuleData: TUsedModuleData);
var
  edgeNode: IXMLNode;
  edgeGraphics: IXMLNode;
  edge: IXMLNode;
  edgeStyle: IXMLNode;
  edgeLabelOutgoing: IXMLNode;
  DependencyViolation: Boolean;
  DependencyViolationText: string;
  OutGoingUnitsCount: Integer;
  IncomingUnitsCount: Integer;
  edgeHint: IXMLNode;
begin
  // prepare data from module usage
  OutGoingUnitsCount  := 0;
  IncomingUnitsCount  := 0;
  DependencyViolation     := false;
  DependencyViolationText := '';
  if Assigned(aUsedModuleData) then
  begin
    OutGoingUnitsCount      := aUsedModuleData.UsingUnitsNames.Count;
    IncomingUnitsCount      := aUsedModuleData.UsedUnitsNames.Count;
    DependencyViolation     := aUsedModuleData.DependencyViolation;
    DependencyViolationText := 'outgoing' +
                               slineBreak +
                               '--------' +
                               slineBreak +
                               aUsedModuleData.UsingUnitsNames.Text + sLineBreak + sLineBreak +
                               'incoming' +
                               slineBreak +
                               '--------' +
                               slineBreak +
                               aUsedModuleData.UsedUnitsNames.Text;
  end;

  // produce the edge
  edgeNode := xmlGraph.AddChild('edge');
  edgeNode.Attributes['id']     := 'e' + IntToStr(edgeID);
  edgeNode.Attributes['source'] := 'n' + IntToStr(CurrentModule.ID);
  edgeNode.Attributes['target'] := 'n' + IntToStr(DependendModule.ID);

  // edge description -> add a description text to the edge line (that will appear as hint in yEd)
  edgeHint                         := edgeNode.AddChild('data');
  edgeHint.Attributes['key']       := cEdgeHintKey;
  edgeHint.Attributes['xml:space'] := 'preserve';
  edgeHint.Text                    := DependencyViolationText;

  // edge graphics
  edgeGraphics                                     := edgeNode.AddChild('data');
  edgeGraphics.Attributes['key']                   := cEdgeKey;
  edge                                             := edgeGraphics.AddChild('y:edge');
  edge.AddChild('y:Arrows').Attributes['target']   := 'standard';

  // styling for the edge
  edgeStyle                                        := edge.AddChild('y:LineStyle');
  if DependencyViolation then
    edgeStyle.Attributes['color']                  := cDependencyViolationColor
  else
    edgeStyle.Attributes['color']                  := cDependencyAllowedColor;

  // add labels to the edge (showing incoming and outgoing units count)
  AddEdgeLabel(edge, DependencyViolation, OutGoingUnitsCount, 'scentr');
  AddEdgeLabel(edge, DependencyViolation, IncomingUnitsCount, 'tcentr');
end;

procedure TExportModulesToGraphML.AppendDependencies(xmlGraph: IXMLNode);
var
  edgeID: Integer;
  CurrentModule: TModule;
  DependendModule: TModule;
  aUsedModuleData: TUsedModuleData;
begin
  edgeID := 0;

  for CurrentModule in FModel.Modules.OrderedModules do
  begin

    // add defined dependencies
    for DependendModule in CurrentModule.DefinedDependencies do
      if ModuleIsPartOfExport(CurrentModule) and ModuleIsPartOfExport(DependendModule)  then
      begin
        CurrentModule.AnalysisData.ActualDependencies.TryGetValue(DependendModule, aUsedModuleData);
        AddEdge(xmlGraph, edgeID, CurrentModule, DependendModule, aUsedModuleData);

        Inc(edgeID);
      end;

    // add dependency violations
    for DependendModule in CurrentModule.AnalysisData.ActualDependencies.Keys do
    begin
      aUsedModuleData := CurrentModule.AnalysisData.ActualDependencies.Items[DependendModule];
      if ModuleIsPartOfExport(CurrentModule) and ModuleIsPartOfExport(DependendModule)  then
        if aUsedModuleData.DependencyViolation then
        begin
          AddEdge(xmlGraph, edgeID, CurrentModule, DependendModule, aUsedModuleData);
          Inc(edgeID);
        end;
    end;

  end;
end;

end.
