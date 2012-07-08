import net.wasamon.javarock.model.VHDLGenericParameter;

import net.wasamon.javarock.model.VHDLPort;
import net.wasamon.javarock.model.VHDLComponentIface;
import net.wasamon.javarock.tools.types.VHDLTypeBuilder;
import net.wasamon.javarock.libraries.VHDLSimpleLibrary;

public class Game extends Thread{
    private int count,cmax;
    private boolean gameover;
    private int pattern, patternBg;
    private int pos, pos1, pos2, vel;
    private int racket0, racket1, mask;
    private int score0, score1;
    private final ButtonIF button = new ButtonIF();

    public boolean isFlag(){
        return gameover;
    }
    public int getPattern(){
        return pattern;
    }
    public int getScore0(){
        return score0;
    }
    public int getScore1(){
        return score1;
    }
    public void run(){
        vel = 1;
        racket0 = 4;
        racket1 = 8388608;
        score0 = 0; score1 = 0;
        gameover = false;
        while(true){
            
            // initialize game.
	    // the previous loser serves the ball and
	    // is given the automatically accelerating racket.
            if (vel == 1) { 
                pos = 1; 
                mask = 1365;
            }
            if (vel ==-1) { 
                pos = 33554432; 
                mask = 2730;
            }
            pos1 = 0;
            pos2 = 0;
            cmax = 700000;
        
            while(gameover==false){
                count++;
                // draw the ball and rackets in background
                patternBg = pos ^ racket0  ^ racket1;
		// draw the ball trajectory by faded lights
		// a faded light is implemented as rapidly blinking light
                if ((count & 1792) == 0) { patternBg = patternBg ^ pos1; }
                if ((count & 1920) == 0) { patternBg = patternBg ^ pos2; }
		// acturally draw the pattern
                pattern = patternBg;
                // players can move the racket
                if(button.pSW1 == false) { racket0 = 4; }
                if(button.pSW0 == false) { racket0 = 2; }
                if(button.pSW2 == false) { racket1 =  8388608; }
                if(button.pSW3 == false) { racket1 = 16777216; }
                // players can bounce the ball by the racket
                if(pos==racket0) {vel =  1;}
                if(pos==racket1) {vel = -1;}
                // players can change the 'parity' of the ball trajectory
                // after hitting the ball.
                // using this technique also causes the ball to accelerate.
                // note that two player has the opposite parity control;
                // this mismatch forces the players to move around
                if(vel==  1 && ((racket0*mask)&pos)!=0) {
                    pos = pos*2;
                    cmax = cmax * 15 / 16;    
                }
                if(vel== -1 && ((racket1/2048*mask)&pos)!=0) {
                    pos = pos/2;
                    cmax = cmax * 15 / 16;
                }

		// time to move the ball
                if(count > cmax){
                    count = 0;
                    //// the ball deccelerates a little when crossing the border.
                    //if((pos & 12288)!=0){
                    //    cmax = cmax * 65 / 64;
                    //}

		    // move the ball and the trajectories
                    if(vel>0){ pos2=pos1; pos1=pos; pos = pos * 4; }
                    if(vel<0){ pos2=pos1; pos1=pos; pos = pos / 4; }

		    // when the ball is out the game is over
                    if(pos < 1){ 
                        pos = 1;
                        score1 += 1;
                        gameover = true; 
                    }
                    if(pos > 33554432){
                        pos = 33554432;
                        score0 += 1; 
                        gameover = true; 
                    }

                }
            } // end of game running loop

            count = 16 * 1048576;
            pos1 = 1;
            while (gameover == true) {
                count--;
                if(count < 0) {gameover = false;}

		// draw the ball and rackets
                patternBg = pos ^ racket0  ^ racket1;
		// draw some congraturating patterns
                patternBg = patternBg ^ (pos1 * 4369 * 32);
                pattern = patternBg;

		// rapidly circulate the congraturating patterns
                if((count&131071)==0) {
                    if(vel > 0) {pos1*=2;}
                    if(vel < 0) {pos1/=2;}
                    if(pos1 < 1){pos1 = 8;}
                    if(pos1 > 8){pos1 = 1;}
                }
            } // end of gameover loop

            // change the velocity so that
	    // previous loser serves the ball
            vel = (-1)*vel;
        }
    }
}
