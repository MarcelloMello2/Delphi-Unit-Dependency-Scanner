object frmSearchAndReplaceUnitName: TfrmSearchAndReplaceUnitName
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Pesquisar e substituir nomes de unidades'
  ClientHeight = 222
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 232
    Height = 13
    Caption = 'Pesquisar por (suporta express'#245'es regulares):'
  end
  object Label2: TLabel
    Left = 8
    Top = 47
    Width = 74
    Height = 13
    Caption = 'Substituir por:'
  end
  object Label3: TLabel
    Left = 8
    Top = 87
    Width = 28
    Height = 13
    Caption = 'Teste:'
  end
  object Label4: TLabel
    Left = 231
    Top = 87
    Width = 55
    Height = 13
    Caption = 'Resultado:'
  end
  object edtSearch: TEdit
    Left = 8
    Top = 21
    Width = 434
    Height = 21
    TabOrder = 0
    OnChange = edtSearchChange
    OnKeyPress = edtSearchKeyPress
  end
  object btnOK: TButton
    Left = 150
    Top = 191
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 230
    Top = 191
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object chkDummyRun: TCheckBox
    Left = 8
    Top = 126
    Width = 248
    Height = 17
    Caption = 'Dummy Run - Nenhum arquivo ser'#225' atualizado'
    TabOrder = 4
  end
  object chkRenameHistoryFiles: TCheckBox
    Left = 8
    Top = 146
    Width = 237
    Height = 17
    Caption = 'Renomear arquivos na pasta "__history"'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object edtReplace: TEdit
    Left = 8
    Top = 61
    Width = 434
    Height = 21
    TabOrder = 1
    OnChange = edtSearchChange
    OnKeyPress = edtSearchKeyPress
  end
  object edtTest: TEdit
    Left = 8
    Top = 101
    Width = 217
    Height = 21
    TabOrder = 2
    OnChange = edtSearchChange
    OnKeyPress = edtSearchKeyPress
  end
  object edtResult: TEdit
    Left = 231
    Top = 101
    Width = 211
    Height = 21
    ReadOnly = True
    TabOrder = 3
    OnChange = edtSearchChange
    OnKeyPress = edtSearchKeyPress
  end
end
