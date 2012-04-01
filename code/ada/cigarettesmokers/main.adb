with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

procedure Main is

   ----- ----- INGREDIENTS ----- -----

   type Ingredient is (Tobacco, Paper, Matches);

   ----- ----- TABLE ----- -----
   protected type Table is
      entry GetTobacco;
      entry GetPaper;
      entry GetMatches;
      entry PlaceTobacco;
      entry PlacePaper;
      entry PlaceMatches;
   private
      TobaccoNum : Integer := 0;
      PaperNum   : Integer := 0;
      MatchesNum : Integer := 0;
   end Table;

   protected body Table is
      -- When taking an ingredient, check we have it
      entry GetTobacco when TobaccoNum > 0 is
      begin
         TobaccoNum := TobaccoNum - 1;
      end GetTobacco;

      entry GetPaper when PaperNum > 0 is
      begin
         PaperNum := PaperNum - 1;
      end GetPaper;

      entry GetMatches when MatchesNum > 0 is
      begin
         MatchesNum := MatchesNum - 1;
      end GetMatches;

      -- When placing an ingredient, check we don't already have two

      entry PlaceTobacco when TobaccoNum < 2 is
      begin
         TobaccoNum := TobaccoNum + 1;
      end PlaceTobacco;

      entry PlacePaper when PaperNum < 2 is
      begin
         PaperNum := PaperNum + 1;
      end PlacePaper;

      entry PlaceMatches when MatchesNum < 2 is
      begin
         MatchesNum := MatchesNum + 1;
      end PlaceMatches;
   end Table;

   ----- ----- AGENT ----- -----

   task type Agent (ID : Integer; IngredA, IngredB : Ingredient; T : access Table);

   task body Agent is
   begin
      while True loop
         -- Put down tobacco if we have it
         if IngredA = Tobacco or IngredB = Tobacco then
            begin
               Put (ID);
               Put_Line (" (agent): I am placing Tobacco");
               T.PlaceTobacco;
            end;
         end if;

         -- Put down paper if we have it
         if IngredA = Paper or IngredB = Paper then
            begin
               Put (ID);
               Put_Line (" (agent): I am placing Paper");
               T.PlacePaper;
            end;
         end if;

         -- Put down matches if we have them
         if IngredA = Matches or IngredB = Matches then
            begin
               Put (ID);
               Put_Line (" (agent): I am placing Matches");
               T.PlaceMatches;
            end;
         end if;
      end loop;
   end Agent;

   ----- ----- ----- Smoker ----- ----- -----

   task type Smoker (ID : Integer; Ingred : Ingredient; T : access Table);

   task body Smoker is
   begin
      while True loop
         -- Take tobacco if we need it
         if Ingred = Paper or Ingred = Matches then
            begin
               Put (ID);
               Put (" (smoker): I am taking Tobacco");
               T.GetTobacco;
            end;
         end if;

         -- Take paper if we need it
         if Ingred = Tobacco or Ingred = Matches then
            begin
               Put (ID);
               Put (" (smoker): I am taking Paper");
               T.GetPaper;
            end;
         end if;

         -- Take matches if we need them
         if Ingred = Tobacco or Ingred = Paper then
            begin
               Put (ID);
               Put (" (smoker): I am taking Matches");
               T.GetMatches;
            end;
         end if;

         -- Smoke
         Put (ID);
         Put_Line (" (smoker): I am smoking");
         delay 1.0;
      end loop;
   end Smoker;

   ----- ----- DECLARATIONS ----- -----

   T : aliased Table;

   A1 : Agent (1, Tobacco, Paper,   T'access);
   A2 : Agent (2, Paper,   Matches, T'access);
   A3 : Agent (3, Tobacco, Matches, T'access);

   S1 : Smoker (1, Tobacco, T'access);
   S2 : Smoker (2, Paper,   T'access);
   S3 : Smoker (3, Matches, T'access);

begin
   null;
end Main;
