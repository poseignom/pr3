program LegacyCSVGenerator;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils;

const
  VERSION = '1.0.0';
  OUTPUT_DIR = '/data/csv';

procedure Log(Message: String);
begin
  WriteLn(Format('%s: %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    Message
  ]));
end;

function GenerateTestData: String;
var
  Timestamp: TDateTime;
  IsActive: Boolean;
  Value: Double;
  Description: String;
  Code: Integer;
begin
  Timestamp := Now;
  IsActive := Random(2) = 1;
  Value := 1000 + Random(5000) / 100;
  Description := 'Запись #' + IntToStr(Random(10000));
  Code := 1000 + Random(9000);
  
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Timestamp) + ';' +
            IfThen(IsActive, 'ИСТИНА', 'ЛОЖЬ') + ';' +
            FormatFloat('0.00', Value) + ';' +
            Description + ';' +
            IntToStr(Code);
end;

procedure GenerateCSV(Count: Integer; FileName: String);
var
  CSVFile: TextFile;
  I: Integer;
begin
  Log('Генерация CSV файла: ' + FileName);
  
  AssignFile(CSVFile, FileName);
  try
    Rewrite(CSVFile);
    
    // Заголовок
    WriteLn(CSVFile, 'Timestamp;IsActive;Value;Description;Code');
    
    // Данные
    for I := 1 to Count do
    begin
      WriteLn(CSVFile, GenerateTestData);
    end;
    
    Log(Format('Сгенерировано %d записей', [Count]));
  finally
    CloseFile(CSVFile);
  end;
end;

procedure CreateHTMLVisualization(CSVFile: String);
var
  HTMLFile: TextFile;
  HTMLFileName: String;
begin
  HTMLFileName := ChangeFileExt(CSVFile, '.html');
  
  AssignFile(HTMLFile, HTMLFileName);
  try
    Rewrite(HTMLFile);
    
    WriteLn(HTMLFile, '<!DOCTYPE html>');
    WriteLn(HTMLFile, '<html>');
    WriteLn(HTMLFile, '<head>');
    WriteLn(HTMLFile, '    <title>Визуализация CSV данных</title>');
    WriteLn(HTMLFile, '    <style>');
    WriteLn(HTMLFile, '        table { border-collapse: collapse; width: 100%; }');
    WriteLn(HTMLFile, '        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }');
    WriteLn(HTMLFile, '        th { background-color: #f2f2f2; }');
    WriteLn(HTMLFile, '        .true { background-color: #d4edda; }');
    WriteLn(HTMLFile, '        .false { background-color: #f8d7da; }');
    WriteLn(HTMLFile, '    </style>');
    WriteLn(HTMLFile, '</head>');
    WriteLn(HTMLFile, '<body>');
    WriteLn(HTMLFile, '    <h1>Визуализация CSV данных</h1>');
    WriteLn(HTMLFile, '    <table>');
    WriteLn(HTMLFile, '        <thead>');
    WriteLn(HTMLFile, '            <tr>');
    WriteLn(HTMLFile, '                <th>Timestamp</th>');
    WriteLn(HTMLFile, '                <th>IsActive</th>');
    WriteLn(HTMLFile, '                <th>Value</th>');
    WriteLn(HTMLFile, '                <th>Description</th>');
    WriteLn(HTMLFile, '                <th>Code</th>');
    WriteLn(HTMLFile, '            </tr>');
    WriteLn(HTMLFile, '        </thead>');
    WriteLn(HTMLFile, '        <tbody>');
    WriteLn(HTMLFile, '            <tr><td colspan="5">Данные загружаются из CSV файла</td></tr>');
    WriteLn(HTMLFile, '        </tbody>');
    WriteLn(HTMLFile, '    </table>');
    WriteLn(HTMLFile, '</body>');
    WriteLn(HTMLFile, '</html>');
    
    Log('Создана HTML визуализация: ' + HTMLFileName);
  finally
    CloseFile(HTMLFile);
  end;
end;

procedure ConvertToXLSX(CSVFile: String);
var
  PythonScript: TStringList;
  ScriptFile: String;
begin
  Log('Конвертация CSV в XLSX...');
  
  PythonScript := TStringList.Create;
  try
    PythonScript.Add('import pandas as pd');
    PythonScript.Add('import sys');
    PythonScript.Add('');
    PythonScript.Add('try:');
    PythonScript.Add('    df = pd.read_csv("' + CSVFile + '", sep=";")');
    PythonScript.Add('    output_file = "' + CSVFile + '".replace(".csv", ".xlsx")');
    PythonScript.Add('    df.to_excel(output_file, index=False)');
    PythonScript.Add('    print("Создан XLSX файл:", output_file)');
    PythonScript.Add('except Exception as e:');
    PythonScript.Add('    print("Ошибка:", e)');
    
    ScriptFile := '/tmp/convert.py';
    PythonScript.SaveToFile(ScriptFile);
    
    // Запуск Python скрипта
    ExecuteProcess('python3', [ScriptFile]);
  finally
    PythonScript.Free;
  end;
end;

var
  Period: Integer;
  CSVFile: String;
begin
  Randomize;
  
  Log('=== Запуск Legacy CSV Generator ' + VERSION + ' ===');
  
  try
    // Создаем директорию если не существует
    if not DirectoryExists(OUTPUT_DIR) then
      CreateDir(OUTPUT_DIR);
    
    // Генерация CSV
    CSVFile := OUTPUT_DIR + '/telemetry_' + 
              FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.csv';
    
    GenerateCSV(50, CSVFile);
    
    // Создание HTML визуализации
    CreateHTMLVisualization(CSVFile);
    
    // Конвертация в XLSX
    ConvertToXLSX(CSVFile);
    
    Log('=== Программа завершена успешно ===');
    
    // Бесконечный цикл для работы в Docker
    Period := 300; // 5 минут
    while True do
    begin
      Sleep(Period * 1000);
      Log('Цикл завершен, ожидание следующего запуска...');
    end;
    
  except
    on E: Exception do
    begin
      Log('Ошибка: ' + E.Message);
      Halt(1);
    end;
  end;
end.