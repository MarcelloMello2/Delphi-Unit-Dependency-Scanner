unit Duds.Modules.Classes.test;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  DUnitX.TestFramework,
  DUnitX.Assert,

  Duds.Common.Types,
  Duds.Common.Interfaces,
  Duds.Modules.Classes;

type
  [TestFixture]
  TDudsModulesSerializerTest = class(TObject)
  private

  public
    [Test]
    procedure ReadFromJson_SimpleTest;

    [Test]
    procedure ReadFromJson_Dependencies;

  end;

implementation

{ TDudsModulesSerializerTest }

procedure TDudsModulesSerializerTest.ReadFromJson_SimpleTest;
const
  json =
    '{' +
    '    "modules": [' +
    '        {' +
    '            "name": "delphi.rtl",' +
    '            "origin": "delphi",' +
    '            "usage": "production",' +
    '            "contains": {' +
    '                "paths": ["C:\\PaTh\\to\\Rtl\\sys", "C:\\PaTh\\to\\Rtl\\common"],' +
    '                "units": ["myUnit", "anotherUnit"]' +
    '            }' +
    '        }' +
    '    ]' +
    '}';
var
  aModulesList: TModulesList;
  aModule: TModule;
begin
  aModulesList := TModulesList.Create;
  try
    TModulesSerializer.ReadFromJson(json, aModulesList);
    Assert.AreEqual(1, aModulesList.Dictionary.Count, 'modules in list');

    aModulesList.Dictionary.TryGetValue('delphi.rtl', aModule);
    Assert.IsNotNull(aModule, 'module ''delphi.rtl'' not found');

    // check name
    Assert.AreEqual('delphi.rtl', aModule.Name, 'modules name');

    // check id
    Assert.AreEqual(1, aModule.ID, 'module id');

    // check origin
    Assert.AreEqual(aModule.Origin, TModuleOrigin.moDelphi, 'module origin');

    // check usage
    Assert.AreEqual(aModule.Usage, TModuleUsage.muProduction, 'module usage');

    // check paths
    Assert.AreEqual(2, aModule.Paths.Count, 'paths in list');

    Assert.AreEqual('c:\path\to\rtl\common\', aModule.Paths[0], false, 'path 1 in sorted list, should be lowercase an inlcude trailing delimiter');
    Assert.AreEqual('c:\path\to\rtl\sys\', aModule.Paths[1], false,    'path 2 in sorted list, should be lowercase an inlcude trailing delimiter');

    // check units
    Assert.AreEqual(2, aModule.Units.Count, 'units in list');

    Assert.AreEqual('anotherunit', aModule.Units[0], false, 'unit 1 in sorted list, should be lowercase');
    Assert.AreEqual('myunit',      aModule.Units[1], false, 'unit 2 in sorted list, should be lowercase');

  finally
    aModulesList.Free;
  end;
end;

procedure TDudsModulesSerializerTest.ReadFromJson_Dependencies;
const
  json =
    '{' +
    '    "modules": [' +
    '        {' +
    '            "name": "common"' +
    '        },' +
        '        {' +
    '            "name": "main",' +
    '            "dependencies": ' +
    '              [' +
    '                "common"' +
    '              ]' +
    '        }' +
    '    ]' +
    '}';
var
  aModulesList: TModulesList;
  aCommonModule: TModule;
  aMainModule: TModule;
begin
  aModulesList := TModulesList.Create;
  try
    TModulesSerializer.ReadFromJson(json, aModulesList);
    Assert.AreEqual(2, aModulesList.Dictionary.Count, 'modules in list');

    aModulesList.Dictionary.TryGetValue('common', aCommonModule);
    Assert.IsNotNull(aCommonModule, 'module ''main'' not found');

    aModulesList.Dictionary.TryGetValue('main', aMainModule);
    Assert.IsNotNull(aMainModule, 'module ''main'' not found');

    // check ids
    Assert.AreEqual(1, aCommonModule.ID, 'module id');
    Assert.AreEqual(2, aMainModule.ID, 'module id');

    // check dependencies
    Assert.AreEqual(1, aMainModule.Dependencies.Count, 'dependencies count in main module');

    Assert.AreEqual(aCommonModule, aMainModule.Dependencies[0], 'dependency to core module');
  finally
    aModulesList.Free;
  end;
end;

end.
