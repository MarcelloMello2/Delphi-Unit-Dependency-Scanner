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

    procedure AppendModuleNodes(xmlGraph: IXMLNode);
    procedure AppendDependencies(xmlGraph: IXMLNode);

  public
    procedure ExportModules;

    property Model: TDudsModel    read FModel    write FModel;
    property OnLog: TLogProcedure read FOnLog    write FOnLog;
    property ProjectSettings: TProjectSettings read fProjectSettings write fProjectSettings;
    property FileName: string read fFileName write fFileName;
  end;


implementation

{ TExportModulesToGraphML }

const
  cNodeKey = 'd0';
  cEdgeKey = 'd1';

const
                                                              {moUndefined,  moDelphi, mo3rdParty,  moOwn}
  cModuleOriginLightColors   : array[TModuleOrigin] of string = ('#CCCCCC', '#FFB565', '#A6D8E7', '#AADB1E');
  cModuleOriginMediumColors  : array[TModuleOrigin] of string = ('#CCCCCC', '#FF8400', '#49AFD9', '#60B515');
  cModuleOriginDarkColors    : array[TModuleOrigin] of string = ('#313131', '#AA4500', '#003D79', '#1D5100');

procedure TExportModulesToGraphML.ExportModules;
var
  Xml: IXMLDocument;
  EdgeID, NodeID: Integer;
  graphml, Key, graph: IXMLNode;
begin
  // Internally GraphML is an XML document
  Xml := TXMLDocument.Create(nil);
  Xml.Active := TRUE;
  Xml.Version := '1.0';
  Xml.Encoding := 'UTF-8';
  Xml.StandAlone := 'no';
  Xml.Options := [doNodeAutoIndent];

  graphml := Xml.AddChild('graphml');
  graphml.Attributes['xmlns:y'] := 'http://www.yworks.com/xml/graphml';
  graphml.Attributes['xmlns:yed'] := 'http://www.yworks.com/xml/yed/3';

  Key := graphml.AddChild('key');
  Key.Attributes['id'] := cNodeKey;
  Key.Attributes['for'] := 'node';
  Key.Attributes['yfiles.type'] := 'nodegraphics';

  Key := graphml.AddChild('key');
  Key.Attributes['id'] := 'd1';
  Key.Attributes['for'] := 'edge';
  Key.Attributes['yfiles.type'] := 'edgegraphics';

  graph := graphml.AddChild('graph');
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

procedure TExportModulesToGraphML.AppendDependencies(xmlGraph: IXMLNode);
var
  edgeNode: IXMLNode;
  edgeID: Integer;
  edgeData: IXMLNode;
  polyLineEdge: IXMLNode;
  CurrentModule: TModule;
  dependendModule: TModule;
begin
  edgeID := 0;

  for CurrentModule in FModel.Modules.OrderedModules do
  begin
    for dependendModule in CurrentModule.Dependencies do
    begin
      edgeNode := xmlGraph.AddChild('edge');
      edgeNode.Attributes['id']     := 'e' + IntToStr(edgeID);
      edgeNode.Attributes['source'] := 'n' + IntToStr(CurrentModule.ID);
      edgeNode.Attributes['target'] := 'n' + IntToStr(dependendModule.ID);

      edgeData := edgeNode.AddChild('data');
      edgeData.Attributes['key'] := 'd1';
      polyLineEdge := edgeData.AddChild('y:PolyLineEdge');

      polyLineEdge.AddChild('y:LineStyle').Attributes['color'] := '#808080';
      polyLineEdge.AddChild('y:Arrows').Attributes['target']   := 'standard';

      Inc(edgeID);
    end;
  end;

end;

end.
