with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

with Ada.Numerics.Float_Random;

procedure Main is
   Num : Integer := 5;

   ----- ----- ----- CHOPSTICK ----- ----- -----

   protected type Chopstick is
      entry Get;
      entry Release;
   private
      Taken : Boolean := False;
   end Chopstick;

   protected body Chopstick is
      entry Get when not Taken is
      begin
         Taken := True;
      end Get;

      entry Release when Taken is
      begin
         Taken := False;
      end Release;
   end Chopstick;

   ----- ----- ----- PHILOSOPHER ----- ----- -----

   task type Philosopher (ID : Integer; L, R : access Chopstick);

   task body Philosopher is
   begin
      while True loop
         -- Get first chopstick
         if ID mod 2 = 0 then
            begin
               Put (ID);
               Put_Line (": I am reaching for my left chopstick");
               L.Get;
            end;
         else
            begin
               Put (ID);
               Put_Line (": I am reaching for my right chopstick");
               R.Get;
            end;
         end if;

         -- Get second chopstick
         if ID mod 2 = 1 then
            begin
               Put (ID);
               Put_Line (": I am reaching for my left chopstick");
               L.Get;
            end;
         else
            begin
               Put (ID);
               Put_Line (": I am reaching for my right chopstick");
               R.Get;
            end;
         end if;

         -- Eat
         Put (ID);
         Put_Line (": I am eating");
         delay 1.0;

         -- Put down
         Put (ID);
         Put_Line (": I am putting down my left chopstick");
         L.Release;

         Put (ID);
         Put_Line (": I am putting down my right chopstick");
         R.Release;

         -- Think
         Put (ID);
         Put_Line (": I am thinking");
         delay 1.0;

         -- Repeat
         Put (ID);
         Put_Line (": I feel hungry again");
      end loop;
   end Philosopher;

   C1, C2, C3, C4, C5 : aliased Chopstick;

   P1 : Philosopher (1, C1'access, C2'access);
   P2 : Philosopher (2, C2'access, C3'access);
   P3 : Philosopher (3, C3'access, C4'access);
   P4 : Philosopher (4, C4'access, C5'access);
   P5 : Philosopher (5, C5'access, C1'access);

begin
   null;
end;
