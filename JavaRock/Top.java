import net.wasamon.javarock.rt.*;

@javarockhdl
    public class Top{
    final Game game = new Game();
    boolean flag;
    int pattern;
    char hex0p;
    char hex1p;
    char hex2p;
    char hex3p;
    char hex4p;
    char hex5p;
    char hex6p;
    char hex7p;
    private char[] font = new char[13];

    @combination
	public int output(){
	return pattern;
    }

    @combination
	public boolean isFlag(){
	return flag;
    }

    @combination
        public char hex0() { return hex0p; }
    @combination
        public char hex1() { return hex1p; }
    @combination
        public char hex2() { return hex2p; }
    @combination
        public char hex3() { return hex3p; }
    @combination
        public char hex4() { return hex4p; }
    @combination
        public char hex5() { return hex5p; }
    @combination
        public char hex6() { return hex6p; }
    @combination
        public char hex7() { return hex7p; }
    

    @auto
	public void main(){
        font[0] = 127 -  63; // 0
        font[1] = 127 -   6; // 1
        font[2] = 127 -  91; // 2
        font[3] = 127 -  79; // 3
        font[4] = 127 - 102; // 4
        font[5] = 127 - 109; // 5
        font[6] = 127 - 125; // 6
        font[7] = 127 -   7; // 7 
        font[8] = 127 - 127; // 8
        font[9] = 127 - 111; // 9
        font[10] = 66;       // G
        font[11] =  8;       // A
        font[12] = 127 - 56; // L


        hex4p = font[0]; 
        hex5p = font[0]; 
        hex6p = font[0]; 
        hex7p = font[0]; 

	game.start();
        while(true) {
            // initialize game
            hex0p = 127;
            hex1p = 127; 
            hex2p = 127; 
            hex3p = 127; 
            while(flag==false){
                pattern = game.getPattern();
                flag = game.isFlag();
            }
            // game over
            hex0p = font[12]; 
            hex1p = font[11]; 
            hex2p = font[0]; 
            hex3p = font[10]; 

            // convert score to decimal
            int score0 = game.getScore0();
            int score1 = game.getScore1();
            int score00 = 0;
            int score01 = 0;
            int score10 = 0;
            int score11 = 0;
            while (score0 > 0) {
                score00++; score0--;
                if (score00>=10) {
                    score00=0; score01++;
                }
            }
            while (score1 > 0) {
                score10++; score1--;
                if (score10>=10) {
                    score10=0; score11++;
                }
            }
            hex4p = font[score00];
            hex5p = font[score01];
            hex6p = font[score10];
            hex7p = font[score11];
            while(flag==true){
                pattern = game.getPattern();
                flag = game.isFlag();
            }
        }
    }
    @unsynthesizable
	public static void main(String[] args){
	Game t = new Game();
	t.start();
    }
}