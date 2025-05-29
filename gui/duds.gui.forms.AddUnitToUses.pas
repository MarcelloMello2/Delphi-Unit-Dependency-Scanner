unit duds.gui.forms.AddUnitToUses;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmAddNewUnit = class(TForm)
    pnlMain: TPanel;
    Label1: TLabel;
    chkDummyRun: TCheckBox;
    edtNewName: TEdit;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    infoRenameCSV: TPanel;
    pnlOnlyForUnits: TPanel;
    Label3: TLabel;
    mem_OnlyApplyToUnits: TMemo;
    Panel5: TPanel;
    Label2: TLabel;
    procedure edtNewNameChange(Sender: TObject);
    procedure edtNewNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TfrmAddNewUnit.edtNewNameChange(Sender: TObject);
begin
  btnOK.Enabled := edtNewName.Text <> '';
end;

procedure TfrmAddNewUnit.edtNewNameKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, [' ']) then
    Key := #00;
end;

end.
