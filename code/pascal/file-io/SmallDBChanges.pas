// Previous code for small db remains unchanged...

// ===========================
// = Loading and Saving Data =
// ===========================

procedure ReadRowFromFile(var toLoad: Row; var input: Text);
begin
    // Load defaults in case Loading fails...
    toLoad.id := 0;
    toLoad.data.kind := INT_VAL;
    toLoad.data.intVal := 0;
    
    // Read in the id and the kind (as an integer)
    ReadLn(input, toLoad.id, toLoad.data.kind);
    
    // Branch based on the kind, and output the data
    case toLoad.data.kind of
        INT_VAL: ReadLn(input, toLoad.data.intVal);
        // Add double as an option
        TXT_VAL: ReadLn(input, toLoad.data.txtVal); // no & as char array
        else
            // Dont know what the value is... set it to 0
            toLoad.data.intVal := 0;
    end;
end;

procedure Load(var dbData: DataStore; filename: String);
var
    input: Text;
    i, rowCount: Integer;
    nextIdBuff: String[8]; // A buffer used to skip "Next id:" from the file
    rowsBuff: String[5]; // A buffer used to skip "rows:"
begin
    Assign(input, filename);
    Reset(input);
    
    // Save the row count...
    ReadLn(input, nextIdBuff, dbData.nextRowId); 
    ReadLn(input, rowsBuff, rowCount);
    
    // Allocate space for rows...
    SetLength(dbData.rows, rowCount);
    
    // For each row in the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        // read the row from the file, into the space allocated for this row
        ReadRowFromFile(dbData.rows[i], input);
    end;        
    
    Close(input);
end;

procedure SaveRow(const toSave: Row; var output: Text);
begin
    WriteLn(output, toSave.id, ' ', toSave.data.kind);
    
    // Branch based on the kind, and output the data
    case toSave.data.kind of
        INT_VAL: WriteLn(output, toSave.data.intVal);
        // Add double as an option
        TXT_VAL: WriteLn(output, toSave.data.txtVal);
        else
            WriteLn(output, ''); //dont save unknown data
    end;
end;

procedure Save(const dbData: DataStore; filename: String);
var
    output: Text;   //Text = textfile
    i:  Integer;
begin
    Assign(output, filename);
    Rewrite(output);
    
    // Save the row count...
    WriteLn(output, 'next id:', dbData.nextRowId);
    WriteLn(output, 'rows:', Length(dbData.rows));
    
    // For each row in the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        SaveRow(dbData.rows[i], output);
    end;        
    
    Close(output);
end;

procedure Main();
var
    opt: MenuOption;
    dbData: DataStore; 
begin
    dbData.nextRowId := 0;
    SetLength(dbData.rows, 0);
    Load(dbData, 'data.db');
    
    {code as before...}
    
    Save(dbData, 'data.db');
end;

begin
    Main();
end.