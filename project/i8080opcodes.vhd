-- created based on http://neil.franklin.ch/Info_Texts/Instruction_Set_8080 

-- 20|30: unused in 8080, RIM and SIM only in 8085
-- 40|49|52|5B|64|6D|7F: are all NOPs
-- 76: would be MOV M,M (3 cycle NOP) but used for HLT

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

package opcodes is

constant opcNOP       :  std_logic_vector(0 to 7) := X"00"; 
constant opcLXI_B     :  std_logic_vector(0 to 7) := X"01"; --  nnnn 
constant opcSTAX_B    :  std_logic_vector(0 to 7) := X"02"; 
constant opcINX_B     :  std_logic_vector(0 to 7) := X"03"; 
constant opcINR_B     :  std_logic_vector(0 to 7) := X"04"; 
constant opcDCR_B     :  std_logic_vector(0 to 7) := X"05"; 
constant opcMVI_B     :  std_logic_vector(0 to 7) := X"06"; --  nn   
constant opcRLC       :  std_logic_vector(0 to 7) := X"07"; 
constant opcDAD_B     :  std_logic_vector(0 to 7) := X"09"; 
constant opcLDAX_B    :  std_logic_vector(0 to 7) := X"0A"; 
constant opcDCX_B     :  std_logic_vector(0 to 7) := X"0B"; 
constant opcINR_C     :  std_logic_vector(0 to 7) := X"0C"; 
constant opcDCR_C     :  std_logic_vector(0 to 7) := X"0D"; 
constant opcMVI_C     :  std_logic_vector(0 to 7) := X"0E"; --  nn   
constant opcRRC       :  std_logic_vector(0 to 7) := X"0F"; 
constant opcLXI_D     :  std_logic_vector(0 to 7) := X"11"; --  nnnn 
constant opcSTAX_D    :  std_logic_vector(0 to 7) := X"12"; 
constant opcINX_D     :  std_logic_vector(0 to 7) := X"13"; 
constant opcINR_D     :  std_logic_vector(0 to 7) := X"14"; 
constant opcDCR_D     :  std_logic_vector(0 to 7) := X"15"; 
constant opcMVI_D     :  std_logic_vector(0 to 7) := X"16"; --  nn   
constant opcRAL       :  std_logic_vector(0 to 7) := X"17"; 
constant opcDAD_D     :  std_logic_vector(0 to 7) := X"19"; 
constant opcLDAX_D    :  std_logic_vector(0 to 7) := X"1A"; 
constant opcDCX_D     :  std_logic_vector(0 to 7) := X"1B"; 
constant opcINR_E     :  std_logic_vector(0 to 7) := X"1C"; 
constant opcDCR_E     :  std_logic_vector(0 to 7) := X"1D"; 
constant opcMVI_E     :  std_logic_vector(0 to 7) := X"1E"; --  nn   
constant opcRAR       :  std_logic_vector(0 to 7) := X"1F"; 
constant opcRIM       :  std_logic_vector(0 to 7) := X"20"; 
constant opcLXI_H     :  std_logic_vector(0 to 7) := X"21"; --  nnnn 
constant opcSHLD      :  std_logic_vector(0 to 7) := X"22"; --  nnnn 
constant opcINX_H     :  std_logic_vector(0 to 7) := X"23"; 
constant opcINR_H     :  std_logic_vector(0 to 7) := X"24"; 
constant opcDCR_H     :  std_logic_vector(0 to 7) := X"25"; 
constant opcMVI_H     :  std_logic_vector(0 to 7) := X"26"; --  nn   
constant opcDAA       :  std_logic_vector(0 to 7) := X"27"; 
constant opcDAD_H     :  std_logic_vector(0 to 7) := X"29"; 
constant opcLHLD      :  std_logic_vector(0 to 7) := X"2A"; --  nnnn 
constant opcDCX_H     :  std_logic_vector(0 to 7) := X"2B"; 
constant opcINR_L     :  std_logic_vector(0 to 7) := X"2C"; 
constant opcDCR_L     :  std_logic_vector(0 to 7) := X"2D"; 
constant opcMVI_L     :  std_logic_vector(0 to 7) := X"2E"; --  nn   
constant opcCMA       :  std_logic_vector(0 to 7) := X"2F"; 
constant opcSIM       :  std_logic_vector(0 to 7) := X"30"; 
constant opcLXI_SP    :  std_logic_vector(0 to 7) := X"31"; --  nnnn 
constant opcSTA       :  std_logic_vector(0 to 7) := X"32"; --  nnnn 
constant opcINX_SP    :  std_logic_vector(0 to 7) := X"33"; 
constant opcINR_M     :  std_logic_vector(0 to 7) := X"34"; 
constant opcDCR_M     :  std_logic_vector(0 to 7) := X"35"; 
constant opcMVI_M     :  std_logic_vector(0 to 7) := X"36"; --  nn   
constant opcSTC       :  std_logic_vector(0 to 7) := X"37"; 
constant opcDAD_SP    :  std_logic_vector(0 to 7) := X"39"; 
constant opcLDA       :  std_logic_vector(0 to 7) := X"3A"; --  nnnn 
constant opcDCX_SP    :  std_logic_vector(0 to 7) := X"3B"; 
constant opcINR_A     :  std_logic_vector(0 to 7) := X"3C"; 
constant opcDCR_A     :  std_logic_vector(0 to 7) := X"3D"; 
constant opcMVI_A     :  std_logic_vector(0 to 7) := X"3E"; --  nn   
constant opcCMC       :  std_logic_vector(0 to 7) := X"3F"; 
constant opcMOV_B_B   :  std_logic_vector(0 to 7) := X"40"; 
constant opcMOV_B_C   :  std_logic_vector(0 to 7) := X"41"; 
constant opcMOV_B_D   :  std_logic_vector(0 to 7) := X"42"; 
constant opcMOV_B_E   :  std_logic_vector(0 to 7) := X"43"; 
constant opcMOV_B_H   :  std_logic_vector(0 to 7) := X"44"; 
constant opcMOV_B_L   :  std_logic_vector(0 to 7) := X"45"; 
constant opcMOV_B_M   :  std_logic_vector(0 to 7) := X"46"; 
constant opcMOV_B_A   :  std_logic_vector(0 to 7) := X"47"; 
constant opcMOV_C_B   :  std_logic_vector(0 to 7) := X"48"; 
constant opcMOV_C_C   :  std_logic_vector(0 to 7) := X"49"; 
constant opcMOV_C_D   :  std_logic_vector(0 to 7) := X"4A"; 
constant opcMOV_C_E   :  std_logic_vector(0 to 7) := X"4B"; 
constant opcMOV_C_H   :  std_logic_vector(0 to 7) := X"4C"; 
constant opcMOV_C_L   :  std_logic_vector(0 to 7) := X"4D"; 
constant opcMOV_C_M   :  std_logic_vector(0 to 7) := X"4E"; 
constant opcMOV_C_A   :  std_logic_vector(0 to 7) := X"4F"; 
constant opcMOV_D_B   :  std_logic_vector(0 to 7) := X"50"; 
constant opcMOV_D_C   :  std_logic_vector(0 to 7) := X"51"; 
constant opcMOV_D_D   :  std_logic_vector(0 to 7) := X"52"; 
constant opcMOV_D_E   :  std_logic_vector(0 to 7) := X"53"; 
constant opcMOV_D_H   :  std_logic_vector(0 to 7) := X"54"; 
constant opcMOV_D_L   :  std_logic_vector(0 to 7) := X"55"; 
constant opcMOV_D_M   :  std_logic_vector(0 to 7) := X"56"; 
constant opcMOV_D_A   :  std_logic_vector(0 to 7) := X"57"; 
constant opcMOV_E_B   :  std_logic_vector(0 to 7) := X"58"; 
constant opcMOV_E_C   :  std_logic_vector(0 to 7) := X"59"; 
constant opcMOV_E_D   :  std_logic_vector(0 to 7) := X"5A"; 
constant opcMOV_E_E   :  std_logic_vector(0 to 7) := X"5B"; 
constant opcMOV_E_H   :  std_logic_vector(0 to 7) := X"5C"; 
constant opcMOV_E_L   :  std_logic_vector(0 to 7) := X"5D"; 
constant opcMOV_E_M   :  std_logic_vector(0 to 7) := X"5E"; 
constant opcMOV_E_A   :  std_logic_vector(0 to 7) := X"5F"; 
constant opcMOV_H_B   :  std_logic_vector(0 to 7) := X"60"; 
constant opcMOV_H_C   :  std_logic_vector(0 to 7) := X"61"; 
constant opcMOV_H_D   :  std_logic_vector(0 to 7) := X"62"; 
constant opcMOV_H_E   :  std_logic_vector(0 to 7) := X"63"; 
constant opcMOV_H_H   :  std_logic_vector(0 to 7) := X"64"; 
constant opcMOV_H_L   :  std_logic_vector(0 to 7) := X"65"; 
constant opcMOV_H_M   :  std_logic_vector(0 to 7) := X"66"; 
constant opcMOV_H_A   :  std_logic_vector(0 to 7) := X"67"; 
constant opcMOV_L_B   :  std_logic_vector(0 to 7) := X"68"; 
constant opcMOV_L_C   :  std_logic_vector(0 to 7) := X"69"; 
constant opcMOV_L_D   :  std_logic_vector(0 to 7) := X"6A"; 
constant opcMOV_L_E   :  std_logic_vector(0 to 7) := X"6B"; 
constant opcMOV_L_H   :  std_logic_vector(0 to 7) := X"6C"; 
constant opcMOV_L_L   :  std_logic_vector(0 to 7) := X"6D"; 
constant opcMOV_L_M   :  std_logic_vector(0 to 7) := X"6E"; 
constant opcMOV_L_A   :  std_logic_vector(0 to 7) := X"6F"; 
constant opcMOV_M_B   :  std_logic_vector(0 to 7) := X"70"; 
constant opcMOV_M_C   :  std_logic_vector(0 to 7) := X"71"; 
constant opcMOV_M_D   :  std_logic_vector(0 to 7) := X"72"; 
constant opcMOV_M_E   :  std_logic_vector(0 to 7) := X"73"; 
constant opcMOV_M_H   :  std_logic_vector(0 to 7) := X"74"; 
constant opcMOV_M_L   :  std_logic_vector(0 to 7) := X"75"; 
constant opcHLT       :  std_logic_vector(0 to 7) := X"76"; 
constant opcMOV_M_A   :  std_logic_vector(0 to 7) := X"77"; 
constant opcMOV_A_B   :  std_logic_vector(0 to 7) := X"78"; 
constant opcMOV_A_C   :  std_logic_vector(0 to 7) := X"79"; 
constant opcMOV_A_D   :  std_logic_vector(0 to 7) := X"7A"; 
constant opcMOV_A_E   :  std_logic_vector(0 to 7) := X"7B"; 
constant opcMOV_A_H   :  std_logic_vector(0 to 7) := X"7C"; 
constant opcMOV_A_L   :  std_logic_vector(0 to 7) := X"7D"; 
constant opcMOV_A_M   :  std_logic_vector(0 to 7) := X"7E"; 
constant opcMOV_A_A   :  std_logic_vector(0 to 7) := X"7F"; 
constant opcADD_B     :  std_logic_vector(0 to 7) := X"80"; 
constant opcADD_C     :  std_logic_vector(0 to 7) := X"81"; 
constant opcADD_D     :  std_logic_vector(0 to 7) := X"82"; 
constant opcADD_E     :  std_logic_vector(0 to 7) := X"83"; 
constant opcADD_H     :  std_logic_vector(0 to 7) := X"84"; 
constant opcADD_L     :  std_logic_vector(0 to 7) := X"85"; 
constant opcADD_M     :  std_logic_vector(0 to 7) := X"86"; 
constant opcADD_A     :  std_logic_vector(0 to 7) := X"87"; 
constant opcADC_B     :  std_logic_vector(0 to 7) := X"88"; 
constant opcADC_C     :  std_logic_vector(0 to 7) := X"89"; 
constant opcADC_D     :  std_logic_vector(0 to 7) := X"8A"; 
constant opcADC_E     :  std_logic_vector(0 to 7) := X"8B"; 
constant opcADC_H     :  std_logic_vector(0 to 7) := X"8C"; 
constant opcADC_L     :  std_logic_vector(0 to 7) := X"8D"; 
constant opcADC_M     :  std_logic_vector(0 to 7) := X"8E"; 
constant opcADC_A     :  std_logic_vector(0 to 7) := X"8F"; 
constant opcSUB_B     :  std_logic_vector(0 to 7) := X"90"; 
constant opcSUB_C     :  std_logic_vector(0 to 7) := X"91"; 
constant opcSUB_D     :  std_logic_vector(0 to 7) := X"92"; 
constant opcSUB_E     :  std_logic_vector(0 to 7) := X"93"; 
constant opcSUB_H     :  std_logic_vector(0 to 7) := X"94"; 
constant opcSUB_L     :  std_logic_vector(0 to 7) := X"95"; 
constant opcSUB_M     :  std_logic_vector(0 to 7) := X"96"; 
constant opcSUB_A     :  std_logic_vector(0 to 7) := X"97"; 
constant opcSBB_B     :  std_logic_vector(0 to 7) := X"98"; 
constant opcSBB_C     :  std_logic_vector(0 to 7) := X"99"; 
constant opcSBB_D     :  std_logic_vector(0 to 7) := X"9A"; 
constant opcSBB_E     :  std_logic_vector(0 to 7) := X"9B"; 
constant opcSBB_H     :  std_logic_vector(0 to 7) := X"9C"; 
constant opcSBB_L     :  std_logic_vector(0 to 7) := X"9D"; 
constant opcSBB_M     :  std_logic_vector(0 to 7) := X"9E"; 
constant opcSBB_A     :  std_logic_vector(0 to 7) := X"9F"; 
constant opcANA_B     :  std_logic_vector(0 to 7) := X"A0"; 
constant opcANA_C     :  std_logic_vector(0 to 7) := X"A1"; 
constant opcANA_D     :  std_logic_vector(0 to 7) := X"A2"; 
constant opcANA_E     :  std_logic_vector(0 to 7) := X"A3"; 
constant opcANA_H     :  std_logic_vector(0 to 7) := X"A4"; 
constant opcANA_L     :  std_logic_vector(0 to 7) := X"A5"; 
constant opcANA_M     :  std_logic_vector(0 to 7) := X"A6"; 
constant opcANA_A     :  std_logic_vector(0 to 7) := X"A7"; 
constant opcXRA_B     :  std_logic_vector(0 to 7) := X"A8"; 
constant opcXRA_C     :  std_logic_vector(0 to 7) := X"A9"; 
constant opcXRA_D     :  std_logic_vector(0 to 7) := X"AA"; 
constant opcXRA_E     :  std_logic_vector(0 to 7) := X"AB"; 
constant opcXRA_H     :  std_logic_vector(0 to 7) := X"AC"; 
constant opcXRA_L     :  std_logic_vector(0 to 7) := X"AD"; 
constant opcXRA_M     :  std_logic_vector(0 to 7) := X"AE"; 
constant opcXRA_A     :  std_logic_vector(0 to 7) := X"AF"; 
constant opcORA_B     :  std_logic_vector(0 to 7) := X"B0"; 
constant opcORA_C     :  std_logic_vector(0 to 7) := X"B1"; 
constant opcORA_D     :  std_logic_vector(0 to 7) := X"B2"; 
constant opcORA_E     :  std_logic_vector(0 to 7) := X"B3"; 
constant opcORA_H     :  std_logic_vector(0 to 7) := X"B4"; 
constant opcORA_L     :  std_logic_vector(0 to 7) := X"B5"; 
constant opcORA_M     :  std_logic_vector(0 to 7) := X"B6"; 
constant opcORA_A     :  std_logic_vector(0 to 7) := X"B7"; 
constant opcCMP_B     :  std_logic_vector(0 to 7) := X"B8"; 
constant opcCMP_C     :  std_logic_vector(0 to 7) := X"B9"; 
constant opcCMP_D     :  std_logic_vector(0 to 7) := X"BA"; 
constant opcCMP_E     :  std_logic_vector(0 to 7) := X"BB"; 
constant opcCMP_H     :  std_logic_vector(0 to 7) := X"BC"; 
constant opcCMP_L     :  std_logic_vector(0 to 7) := X"BD"; 
constant opcCMP_M     :  std_logic_vector(0 to 7) := X"BE"; 
constant opcCMP_A     :  std_logic_vector(0 to 7) := X"BF"; 
constant opcRNZ       :  std_logic_vector(0 to 7) := X"C0"; 
constant opcPOP_B     :  std_logic_vector(0 to 7) := X"C1"; 
constant opcJNZ_nn    :  std_logic_vector(0 to 7) := X"C2"; --  nnnn 
constant opcJMP_nn    :  std_logic_vector(0 to 7) := X"C3"; --  nnnn 
constant opcCNZ_nn    :  std_logic_vector(0 to 7) := X"C4"; --  nnnn 
constant opcPUSH_B    :  std_logic_vector(0 to 7) := X"C5"; 
constant opcADI       :  std_logic_vector(0 to 7) := X"C6"; --  nn   
constant opcRST_0     :  std_logic_vector(0 to 7) := X"C7"; 
constant opcRZ        :  std_logic_vector(0 to 7) := X"C8"; 
constant opcRET       :  std_logic_vector(0 to 7) := X"C9"; 
constant opcJZ        :  std_logic_vector(0 to 7) := X"CA"; --  nnnn 
constant opcCZ        :  std_logic_vector(0 to 7) := X"CC"; --  nnnn 
constant opcCALL      :  std_logic_vector(0 to 7) := X"CD"; --  nnnn 
constant opcACI       :  std_logic_vector(0 to 7) := X"CE"; --  nn   
constant opcRST_1     :  std_logic_vector(0 to 7) := X"CF"; 
constant opcRNC       :  std_logic_vector(0 to 7) := X"D0"; 
constant opcPOP_D     :  std_logic_vector(0 to 7) := X"D1"; 
constant opcJNC       :  std_logic_vector(0 to 7) := X"D2"; --  nnnn 
constant opcOUT      :  std_logic_vector(0 to 7) := X"D3"; --  nn   
constant opcCNC       :  std_logic_vector(0 to 7) := X"D4"; --  nnnn 
constant opcPUSH_D    :  std_logic_vector(0 to 7) := X"D5"; 
constant opcSUI       :  std_logic_vector(0 to 7) := X"D6"; --  nn   
constant opcRST_2     :  std_logic_vector(0 to 7) := X"D7"; 
constant opcRC        :  std_logic_vector(0 to 7) := X"D8"; 
constant opcJC        :  std_logic_vector(0 to 7) := X"DA"; --  nnnn 
constant opcIN        :  std_logic_vector(0 to 7) := X"DB"; --  nn   
constant opcCC        :  std_logic_vector(0 to 7) := X"DC"; --  nnnn 
constant opcSBI       :  std_logic_vector(0 to 7) := X"DE"; --  nn   
constant opcRST_3     :  std_logic_vector(0 to 7) := X"DF"; 
constant opcRPO       :  std_logic_vector(0 to 7) := X"E0"; 
constant opcPOP_H     :  std_logic_vector(0 to 7) := X"E1"; 
constant opcJPO       :  std_logic_vector(0 to 7) := X"E2"; --  nnnn 
constant opcXTHL      :  std_logic_vector(0 to 7) := X"E3"; 
constant opcCPO       :  std_logic_vector(0 to 7) := X"E4"; --  nnnn 
constant opcPUSH_H    :  std_logic_vector(0 to 7) := X"E5"; 
constant opcANI       :  std_logic_vector(0 to 7) := X"E6"; --  nn   
constant opcRST_4     :  std_logic_vector(0 to 7) := X"E7"; 
constant opcRPE       :  std_logic_vector(0 to 7) := X"E8"; 
constant opcPCHL      :  std_logic_vector(0 to 7) := X"E9"; 
constant opcJPE       :  std_logic_vector(0 to 7) := X"EA"; --  nnnn 
constant opcXCHG      :  std_logic_vector(0 to 7) := X"EB"; 
constant opcCPE       :  std_logic_vector(0 to 7) := X"EC"; --  nnnn 
constant opcXRI       :  std_logic_vector(0 to 7) := X"EE"; --  nn   
constant opcRST_5     :  std_logic_vector(0 to 7) := X"EF"; 
constant opcRP        :  std_logic_vector(0 to 7) := X"F0"; 
constant opcPOP_PSW   :  std_logic_vector(0 to 7) := X"F1"; 
constant opcJP        :  std_logic_vector(0 to 7) := X"F2"; --  nnnn 
constant opcDI        :  std_logic_vector(0 to 7) := X"F3"; 
constant opcCP        :  std_logic_vector(0 to 7) := X"F4"; --  nnnn 
constant opcPUSH_PSW  :  std_logic_vector(0 to 7) := X"F5"; 
constant opcORI       :  std_logic_vector(0 to 7) := X"F6"; --  nn   
constant opcRST_6     :  std_logic_vector(0 to 7) := X"F7"; 
constant opcRM        :  std_logic_vector(0 to 7) := X"F8"; 
constant opcSPHL      :  std_logic_vector(0 to 7) := X"F9"; 
constant opcJM        :  std_logic_vector(0 to 7) := X"FA"; --  nnnn 
constant opcEI        :  std_logic_vector(0 to 7) := X"FB"; 
constant opcCM        :  std_logic_vector(0 to 7) := X"FC"; --  nnnn 
constant opcCPI       :  std_logic_vector(0 to 7) := X"FE"; --  nn   
constant opcRST_7     :  std_logic_vector(0 to 7) := X"FF"; 

end opcodes;

package body opcodes is

end opcodes;