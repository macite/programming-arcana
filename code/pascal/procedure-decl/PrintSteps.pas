program PrintSteps; // Prints out the steps to cook a meal... (partial)

procedure FindWhatToCook();
begin
    WriteLn("Step 1 - Find what to cook -");
    WriteLn("1 - Find a recipe book");
    WriteLn("2 - Pick recipe ");
end;

procedure PurchaseMissingIngredients();
begin
  WriteLn("Step 2(a) - Purchase Missing Ingredients -");
  WriteLn("1 - Goto Shop");
  WriteLn("2 - Find ingredients and put in basket");
  WriteLn("3 - Go to register and pay for ingredients in basket");
  WriteLn("4 - Return home");
end;

procedure GetIngredients();
begin
  WriteLn("Step 2 - Purchase Ingredients -");
  WriteLn("1 - Read recipe");
  WriteLn("2 - Write a list of ingredients ");
  WriteLn("3 - Check food stocks, and tick off ingredients you already have");
  PurchaseMissingIngredients();
end;

procedure Main();
begin
    FindWhatToCook();
    GetIngredients();
    // etc...
end;

begin
    Main();
end.