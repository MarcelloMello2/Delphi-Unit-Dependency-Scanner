unit duds.export.units.GraphML;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  Xml.XMLDoc, Xml.XMLIntf,

  duds.common.Types,
  duds.common.Interfaces,
  duds.common.Strings,
  duds.common.Classes,
  duds.analyzer.model;

procedure ExportUnitsToGraphML(Model: TDudsModel; ExportUnitsNotInPath: Boolean; const Filename: String; OnlyModule: TModule = nil);

implementation

procedure ExportUnitsToGraphML(Model: TDudsModel; ExportUnitsNotInPath: Boolean; const Filename: String; OnlyModule: TModule = nil);

  function UnitIsPartOfExport(UnitInfo: IUnitInfo): Boolean;
  begin
    Result := (OnlyModule = nil) or (UnitInfo.Module = OnlyModule);
  end;

type
  TUseSection = (usInterface, usImplementation);

const
  COL: array [TUseSection] of Cardinal = ($000000, $808080);

var
  Xml: IXMLDocument;
  EdgeID, NodeID: Integer;
  EdgeNode1, EdgeNode2: String;
  graphml, Key, graph, Node, edge, line, data: IXMLNode;
  UnitName, TrimmedUnitName, ColourString: String;
  NodeDictionary: TDictionary<String, String>;
  UnitInfo: IUnitInfo;
  UsedUnit: IUsedUnitInfo;
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
  Key.Attributes['id'] := 'd0';
  Key.Attributes['for'] := 'node';
  Key.Attributes['yfiles.type'] := 'nodegraphics';

  Key := graphml.AddChild('key');
  Key.Attributes['id'] := 'd1';
  Key.Attributes['for'] := 'edge';
  Key.Attributes['yfiles.type'] := 'edgegraphics';

  graph := graphml.AddChild('graph');
  graph.Attributes['edgedefault'] := 'directed';

  EdgeID := 0;
  NodeID := 0;

  NodeDictionary := TDictionary<String, String>.Create;
  try
    // build Dictionary & export nodes
    for UnitName in Model.ParsedDelphiFiles.Keys do
    begin
      if (Model.ParsedDelphiFiles[UnitName].InSearchPath) or ExportUnitsNotInPath then
      begin
        UnitInfo := Model.ParsedDelphiFiles[UnitName].UnitInfo;

        if UnitIsPartOfExport(UnitInfo) then
        begin
          TrimmedUnitName := Trim(UnitInfo.DelphiUnitName);

          Node := graph.AddChild('node');
          Node.Attributes['id'] := 'n' + IntToStr(NodeID);

          data := Node.AddChild('data');
          data.Attributes['key'] := 'd0';
          data.AddChild('y:ShapeNode').AddChild('y:NodeLabel').Text := TrimmedUnitName;

          NodeDictionary.AddOrSetValue(UpperCase(TrimmedUnitName), 'n' + NodeID.ToString);

          Inc(NodeID);
        end;
      end;
    end;

    // export edges
    for UnitName in Model.ParsedDelphiFiles.Keys do
    begin
      if (Model.ParsedDelphiFiles[UnitName].InSearchPath) or ExportUnitsNotInPath then
      begin
        UnitInfo := Model.ParsedDelphiFiles[UnitName].UnitInfo;
        if UnitIsPartOfExport(UnitInfo) then
        begin
          TrimmedUnitName := Trim(UnitInfo.DelphiUnitName);

          for UsedUnit in UnitInfo.UsedUnits do
          begin
            if (Model.ParsedDelphiFiles.ContainsKey(UpperCase(UsedUnit.DelphiUnitName))) and
              (ExportUnitsNotInPath or
              (Model.ParsedDelphiFiles[UpperCase(UsedUnit.DelphiUnitName)].InSearchPath)) and
              (NodeDictionary.TryGetValue(UpperCase(TrimmedUnitName), EdgeNode1)) and
              (NodeDictionary.TryGetValue(UpperCase(Trim(UsedUnit.DelphiUnitName)),
              EdgeNode2)) then
            begin
              edge := graph.AddChild('edge');
              edge.Attributes['id'] := 'e' + IntToStr(EdgeID);
              edge.Attributes['source'] := EdgeNode1;
              edge.Attributes['target'] := EdgeNode2;

              data := edge.AddChild('data');
              data.Attributes['key'] := 'd1';
              line := data.AddChild('y:PolyLineEdge');

              case UsedUnit.UsesType of
                utImplementation:
                  ColourString := '000000'
              else
                ColourString := '808080';
              end;

              line.AddChild('y:LineStyle').Attributes['color'] := '#' + ColourString;
              line.AddChild('y:Arrows').Attributes['target']   := 'standard';

              Inc(EdgeID);
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(NodeDictionary);
  end;

  Xml.SaveToFile(Filename);
  Xml.Active := FALSE;
end;

end.
