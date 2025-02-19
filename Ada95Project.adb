with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is

   ---- Global variables ----

   Number_Of_Producers: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;

   type Producer_Type is (Wing, Engine, Fuselage, Tail, LandingGear);
   type Assembly_Type is (Aircabin, Bodywing, Landgear);
   type Consumer_Type is (Airbus, Boeing);
   
   Consumer_Name: constant array (Consumer_Type) of String(1 .. 6)
        := ("Airbus", "Boeing");

   Product_Name: constant array (Producer_Type) of String(1 .. 10)
     := ("Wing      ", "Engine    ", "Fuselage  ", "Tail      ", "Parachutes");

   -- Assembly
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 8)
     := ("Aircabin", "Bodywing", "Landgear");

   ----Task declarations----

   -- Producer
   task type Producer is
      entry Start(Product: in Producer_Type; Production_Time: in Integer);
   end Producer;

   -- Consumer
   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   -- Buffer
   task type Buffer is
      -- Accept assembly if there is place for it
      entry MoveToBuffer(Product: in Producer_Type; Number: in Integer);
      -- Deliver assebly
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
      -- Cleaning day entry
      entry Cleaning_day;
   end Buffer;

   --Buffer Cleaning
   task type Cleaning is
      entry Start;
   end Cleaning;

   -- Creating Tasks
   P: array (Producer_Type) of Producer;
   K: array (Consumer_Type) of Consumer;
   B: Buffer;
   CLEAN: Cleaning;

   ----Task Definitions----

   -- Producer
   task body Producer is
      subtype Production_Time_Range is Integer range 2 .. 4;
      package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_Production.Generator;
      Producer_Type_Number: Producer_Type;
      Product_Number: Integer := 1;
      Production: Integer;
      Random_Time: Duration;
   begin
      accept Start(Product: in Producer_Type; Production_Time: in Integer) do
         Random_Production.Reset(G);
         Producer_Type_Number := Product;
         Production := Production_Time;
      end Start;
      Put_Line(ESC & "[93m" & "P: Started producer of " & Product_Name(Producer_Type_Number) & ESC & "[0m");
      loop
         Random_Time := Duration(Random_Production.Random(G));
         delay Random_Time;
         Put_Line(ESC & "[93m" & "P: Produced part " & Product_Name(Producer_Type_Number)
                  & " number " & Integer'Image(Product_Number) & ESC & "[0m");
         select
            B.MoveToBuffer(Producer_Type_Number, Product_Number);
         or
            delay 2.0; -- Timeout after 2 units of time
            Put_Line(ESC & "[93m" & "P: Timeout moving to buffer part " & Product_Name(Producer_Type_Number)
                     & " number " & Integer'Image(Product_Number) & " - Retrying." & ESC & "[0m");
         end select;
         Product_Number := Product_Number + 1;
      end loop;
   end Producer;

   -- Consumer
   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);

      -- Every consumer wants a random assembly
      package Random_Assembly is new
        Ada.Numerics.Discrete_Random(Assembly_Type);

      G: Random_Consumption.Generator;
      GA: Random_Assembly.Generator;
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Kind: Assembly_Type;
      
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly.Reset(GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line(ESC & "[96m" & "C: Started consumer " & Consumer_Name(Consumer_Nb) & ESC & "[0m");
      loop
         delay Duration(Random_Consumption.Random(G)); -- Simulating consumption
         Assembly_Kind := Random_Assembly.Random(GA);
         -- Delivering to client
         B.Deliver(Assembly_Kind, Assembly_Number);
         if Assembly_Number /= 0 then
            Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " gets assembly delivered: " &
                      Assembly_Name(Assembly_Kind) & " number " &
                      Integer'Image(Assembly_Number) & ESC & "[0m");
         else
            Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " - no assembly available." & ESC & "[0m");
         end if;
      end loop;
   end Consumer;

   -- Buffer
   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Producer_Type) of Integer;
      Storage: Storage_type := (others => 0);
      Assembly_Content: array(Assembly_Type, Producer_Type) of Integer
        := ((Wing => 2, Engine => 1, Fuselage => 2, Tail => 2, LandingGear => 0),
            (Wing => 1, Engine => 1, Fuselage => 0, Tail => 1, LandingGear => 1),
            (Wing => 3, Engine => 0, Fuselage => 2, Tail => 0, LandingGear => 2));
      Max_Assembly_Content: array(Producer_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer := (others => 1);
      In_Storage: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Producer_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Product: Producer_Type) return Boolean is
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         else
            return True;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Producer_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         Put_Line("STORAGE (Total Products: " & Integer'Image(In_Storage) & ")");

         for W in Producer_Type loop
            Put_Line(Integer'Image(Storage(W)) & " " & Product_Name(W));
         end loop;
         
      end Storage_Contents;

      -- Procedure for cleaning the buffer (also prevents the buffer getting stuck)
      procedure Today_Is_Cleaning_Day is
      begin
         Put_Line("B: Today is cleaning day. Removing 3 parts of each type.");
         for W in Producer_Type loop
            if Storage(W) >= 3 then
               Storage(W) := Storage(W) - 3;
               In_Storage := In_Storage - 3;
               Put_Line("B: Removed 3 parts of " & Product_Name(W));
            else
               Put_Line("B: Not enough parts of " & Product_Name(W) & " to remove.");
            end if;
         end loop;
      end Today_Is_Cleaning_Day;

   begin
      Put_Line(ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
      Setup_Variables;
      loop
         select
            accept MoveToBuffer(Product: in Producer_Type; Number: in Integer) do
               if Can_Accept(Product) then
                  Put_Line(ESC & "[91m" & "B: Accepted part " & Product_Name(Product) & " number " &
                            Integer'Image(Number)& ESC & "[0m");
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
                  Put_Line(ESC & "[91m" & "B: Rejected part " & Product_Name(Product) & " number " &
                             Integer'Image(Number)& ESC & "[0m");
               end if;
            end MoveToBuffer;
         or
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
               if Can_Deliver(Assembly) then
                  Put_Line(ESC & "[91m" & "B: Delivered assembly " & Assembly_Name(Assembly) & " number " &
                            Integer'Image(Assembly_Number(Assembly))& ESC & "[0m");
                  for W in Producer_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  Put_Line(ESC & "[91m" & "B: Lacking parts for assembly " & Assembly_Name(Assembly)& ESC & "[0m");
                  Number := 0;
               end if;
            end Deliver;
         or
            accept Cleaning_day do
               Today_Is_Cleaning_Day;
               end Cleaning_day;
         end select;
         Storage_Contents;
      end loop;
   end Buffer;

   -- Cleaning day task body
   task body Cleaning is
      day_number: Integer := 1;
   begin
      accept Start do
         Put_Line("Cleaning: Started cleaning task.");
      end Start;
      loop
         delay 3.0; -- Length of 1 day
         Put_Line("Cleaning: Day " & Integer'Image(day_number));
         if day_number = 10 then
            Put_Line("Cleaning: Initiating cleaning day.");
            B.Cleaning_day;
            day_number := 1;
         else
            day_number := day_number + 1;
         end if;
      end loop;
   end Cleaning;

   ----Main loop----
begin
   for I in Producer_Type'Range loop
      P(I).Start(I, 10);
   end loop;
   for J in Consumer_Type'Range loop
      K(J).Start(J, 12);
   end loop;
   CLEAN.Start;
end Simulation;