#------------------------------------------------------------------------------------------------------------
# V teto casti jsou zakladni testy, ktere jsou soucasti zadani
# ===========================================================================================================
# Zasah do teto casti neni dovolen. V pripade, ze potrebujete otestovat funkcnost lepe, zduplikujte si kod 
# testu do uzivatelske casti tohoto skriptu a dopiste pozadovanou funkcionalitu
#------------------------------------------------------------------------------------------------------------

from scripts.cpu_lib import *

@tb_test()
async def test_reset(dut):
    clk_gen = await cpu_dut_init(dut)
    #aktivovat reset, pockat 50ns, deaktivovat reset
    dut.reset.value = 1
    await Timer(50, units='ns')
    dut.reset.value = 0

    #vyhodnotit stav signalu
    assert (dut.data_en.value.binstr in ['0', '1']), "Invalid enable value"
    assert (dut.done.value.binstr == '0'), "Invalid done value"
    assert (dut.ready.value.binstr == '0'), "Invalid ready value"
    assert ((dut.data_en.value.binstr == '0') or ((dut.data_en.value.binstr == '1') and (dut.data_rdwr.value.binstr == '1'))), "Processor should read from the memory"
    assert (dut.in_req.value.binstr == '0'), "Can't request for input data"
    assert (dut.out_we.value.binstr == '0'), "Can't write to the output" 

@tb_test()
async def test_init(dut):
    """Procesor initialization test"""
    instcnt, mem, _ = await run_program(dut, '@')
    assert (dut.ready.value.binstr == '1'), "Invalid ready value, should be active"
    

@tb_test()
async def test_increment(dut):
    """Increment value of the first memory cell, i.e. *ptr++"""
    instcnt, mem, _ = await run_program(dut, '+++@')
    assert (dut.done.value.binstr == '1'), "Invalid done value, should be active"
    assert instcnt == 4
    assert mem[4] == 3


@tb_test()
async def test_decrement(dut):
    """Decrement value of the first memory cell, i.e. *ptr--"""
    instcnt, mem, _ = await run_program(dut, '---@\3')
    assert instcnt == 4
    assert mem[4] == 0


@tb_test()
async def test_move(dut):
    """Move the pointer to the next cell and increment its value"""
    instcnt, mem, _ = await run_program(dut, '>+@')
    assert instcnt == 3
    assert mem[3] == 0
    assert mem[4] == 1

@tb_test()
async def test_print(dut):
    """Print data to the output, i.e. putchar(*ptr)"""
    instcnt, mem, lcd = await run_program(dut, '...@012', timeout_ns=LCD_WAIT_TIME*5 + 1000)
    assert lcd == "000"

@tb_test()
async def test_input(dut):
    """Load data from the input, i.e. *ptr=getchar()"""
    instcnt, mem, lcd = await run_program(dut, ',,,@', kb_data='312', timeout_ns=(KB_WAIT_TIME+LCD_WAIT_TIME)*5 + 100)
    assert mem[4] == ord('2')

    instcnt, mem, lcd = await run_program(dut, ',,,@', kb_data='123', timeout_ns=(KB_WAIT_TIME+LCD_WAIT_TIME)*5 + 100)
    assert mem[4] == ord('3')


@tb_test()
async def test_while_loop(dut):
    """Simple while loop test"""
    instcnt, mem, lcd = await run_program(dut, '[.-]@\3', timeout_ns = LCD_WAIT_TIME*10)
    assert mem[5] == 0
    assert lcd == "\3\2\1"


@tb_test()
async def test_tmp(dut):
    """Simple temp register test"""
    instcnt, mem, lcd = await run_program(dut, '$+++!@\1', timeout_ns = LCD_WAIT_TIME*10)
    assert mem[6] == 1

@tb_test()
async def test_login(dut, uid=''):
    """Executes program in login.b file"""
    file_name = '../src/login.b'
    assert os.path.isfile(file_name), "File login.b is missing" 
    with open(file_name,'rt') as f:
        prog = f.read()
    assert len(prog), "File login.b doesn't contain any program"
    instcnt, mem, lcd = await run_program(dut, prog, timeout_ns = 250_000)
    lcd = lcd.lower()
    assert lcd == uid, "Invalid output"


#-------------------------------------------------------------------------------------------------------
# Uzivatelske testy
# ===========================================================================================================
# V teto casti muzete v pripade potreby vlozit jakekoliv vlastni testy. 
#-------------------------------------------------------------------------------------------------------


#Odkomentujte jeden z nasledujicich radku pro zarazeni do testu
#@tb_test(skip=True) #test se nespousti automaticky, spustit lze volanim TESTCASE=test_printf make
#@tb_test() #test se spousti automaticky
async def test_printf(dut):
    """Program which emulates printing of %d """

    #Priklad testu, ktery provede pozadovany program:
    #Program ocekava vstup z klavesnice, v testu je stisknuta klavesa odpovidajici hodnote 123
    prog = ',#load value and print#[>>+>+<<<-]>>>[<<<+>>>-]<<+>[<->[>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]++++++++[<++++++>-]>[<<+>>-]>[<<+>>-]<<]>]<[->>++++++++[<++++++>-]]<[.[-]<]<@'
    enableDebug(lcd=True)
    instcnt, mem, lcd = await run_program(dut, prog, kb_data=chr(123), timeout_ns = 1_000_000)
    assert lcd == '123', "Invalid output"



if __name__=="__main__":
  # call the main function
  print("Run this simulation by calling make.")
  exit()
