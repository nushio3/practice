#include <iostream>
#include <cmath>
#include <sstream>
#include <vector>
#include "SDL.h"
#include "SDL_main.h"
#include "SDL_mouse.h"

/* linker options: -lmingw32 -lSDLmain -lSDL -mwindows */

using namespace std;

unsigned int sampleFrequency = 0;
unsigned int audioBufferSize = 0;
unsigned int outputAudioBufferSize = 0;
Uint8 *hum_buf8;
Uint32 hum_len;
SDL_AudioSpec hum_spec;
int hum_ptr;
double hum_amp=0.1;
double hum_amp_tgt = 0.1;

vector<Uint8*>clash_buf8(8);
Uint32 clash_len[8];
int clash_kind = 7;
int clash_ptr = 0;

int clash_kind_mouse = 4;

double freq1 = 800;
double fase1 = 0;


int mouse_xlo = 90,mouse_ylo = 90;
int mouse_xhi = 1000,mouse_yhi = 1000;

int mouse_x = 100,mouse_y = 100;
int mouse_qx = 100,mouse_qy = 100;
int mouse_vx = 100,mouse_vy = 100;

void example_mixaudio(void *unused, Uint8 *stream8, int len) {

  double bytesPerPeriod1 = sampleFrequency / freq1;
  Uint16 *stream = (Uint16*)(stream8);
  int16_t *hum_buf16 = (int16_t*)(hum_buf8);
  
  for (int i=0;i<len/2;i++) {
    int channel1 = 0*int(12050*sin(fase1*6.28/bytesPerPeriod1));
    int channel2 = 10* hum_amp *  hum_buf16[hum_ptr];
    int outputValue = channel1 +channel2;//
    if (clash_kind >=0) {
      int16_t *clash_buf16 = (int16_t*)(clash_buf8[clash_kind]);
      outputValue += clash_buf16[clash_ptr];
      clash_ptr++;
      if(clash_ptr >= clash_len[clash_kind]/2-1){
	clash_kind=-1;
      }
    }
    //+channel2/4;

    // just add the channels
    if (outputValue > 32767) outputValue = 32767;        // and clip the result
    if (outputValue < -32768) outputValue = -32768;      // this seems a crude method, but works very well

    stream[i] = outputValue;

    fase1++;
    //fase1 %= bytesPerPeriod1;
    hum_ptr ++;
    hum_ptr %= hum_len/2;
    if (hum_amp > hum_amp_tgt)
      hum_amp = max(hum_amp_tgt, hum_amp*(1-2.0/sampleFrequency));
    if (hum_amp < hum_amp_tgt)
      hum_amp = min(hum_amp_tgt, hum_amp*(1+20.0/sampleFrequency));
    hum_amp_tgt = max(0.1, hum_amp_tgt*(1-2.0/sampleFrequency));
  }


  SDL_GetMouseState(&mouse_x, &mouse_y);
  mouse_vx = mouse_x-mouse_qx;
  mouse_vy = mouse_y-mouse_qy;
  
  mouse_qx=mouse_x;
  mouse_qy=mouse_y;
  
  
  hum_amp_tgt=max(0.1,
		  min(1.0,
		  max(hum_amp_tgt,
		      sqrt(mouse_vx*mouse_vx+mouse_vy*mouse_vy)/256.0)));
  mouse_xlo = min (mouse_x + 80, mouse_xlo);
  mouse_xhi = max (mouse_x - 80, mouse_xhi);
  mouse_ylo = min (mouse_y + 80, mouse_ylo);
  mouse_yhi = max (mouse_y - 80, mouse_yhi);

  if (mouse_x <= mouse_xlo && clash_kind < 0) {
    clash_kind = 0;
    clash_ptr = 0;
  }
   if (mouse_x >= mouse_xhi && clash_kind < 0) {
    clash_kind = 1;
    clash_ptr = 0;
  }
  if (mouse_y <= mouse_ylo && clash_kind < 0) {
    clash_kind = 2;
    clash_ptr = 0;
  }
  if (mouse_y >= mouse_yhi && clash_kind < 0) {
    clash_kind = 3;
    clash_ptr = 0;
    }

  ostringstream ostr;
  ostr << mouse_x << " " << mouse_y  << " " << hum_amp_tgt;
  SDL_WM_SetCaption(ostr.str().c_str(),0);	

}

int main(int argc, char *argv[])
{

  if( SDL_Init(SDL_INIT_TIMER | SDL_INIT_AUDIO ) <0 ) {
    cout << "Unable to init SDL: " << SDL_GetError() << endl;
    return 1;
  }

  /* setup audio */
  SDL_AudioSpec *desired, *obtained;

  /* Allocate a desired SDL_AudioSpec */
  desired = (SDL_AudioSpec *) malloc(sizeof(SDL_AudioSpec));

  /* Allocate space for the obtained SDL_AudioSpec */
  obtained = (SDL_AudioSpec *) malloc(sizeof(SDL_AudioSpec));

  /* choose a samplerate and audio-format */
  desired->freq = 44100;
  desired->format = AUDIO_S16;

  /* Large audio buffers reduces risk of dropouts but increases response time.
   *
   * You should always check if you actually GOT the audiobuffer size you wanted,
   * note that not hardware supports all buffer sizes (< 2048 bytes gives problems with some
   * hardware). Older versions of SDL had a bug that caused many configuration to use a
   * buffersize of 11025 bytes, if your sdl.dll is approx. 1 Mb in stead of 220 Kb, download
   * v1.2.8 of SDL or better...)
   */
  desired->samples = 1024;//4096;

  /* Our callback function */
  desired->callback=example_mixaudio;
  desired->userdata=NULL;

  desired->channels = 1;

  /* Open the audio device and start playing sound! */
  if ( SDL_OpenAudio(desired, obtained) < 0 ) {
    fprintf(stderr, "AudioMixer, Unable to open audio: %s\n", SDL_GetError());
    exit(1);
  }

  audioBufferSize = obtained->samples;
  sampleFrequency = obtained->freq;

  /* if the format is 16 bit, two bytes are written for every sample */
  if (obtained->format==AUDIO_U16 || obtained->format==AUDIO_S16) {
    outputAudioBufferSize = 2*audioBufferSize;
  } else {
    outputAudioBufferSize = audioBufferSize;
  }

  SDL_LoadWAV("c0.wav", &hum_spec, &(clash_buf8[0]), clash_len);
  SDL_LoadWAV("c1.wav", &hum_spec, &(clash_buf8[1]), clash_len+1);
  SDL_LoadWAV("c2.wav", &hum_spec, &(clash_buf8[2]), clash_len+2);
  SDL_LoadWAV("c3.wav", &hum_spec, &(clash_buf8[3]), clash_len+3);

  SDL_LoadWAV("s0.wav", &hum_spec, &(clash_buf8[4]), clash_len+4);
  SDL_LoadWAV("s1.wav", &hum_spec, &(clash_buf8[5]), clash_len+5);
  SDL_LoadWAV("s2.wav", &hum_spec, &(clash_buf8[6]), clash_len+6);
  SDL_LoadWAV("SaberOn.wav", &hum_spec, &(clash_buf8[7]), clash_len+7);

  if(SDL_LoadWAV("HumLoop.wav", &hum_spec, &hum_buf8, &hum_len)==NULL){
    fprintf(stderr, "AudioMixer, Unable to load wav: %s\n", SDL_GetError());
    exit(1);
  }


  SDL_Surface *screen = SDL_SetVideoMode(1024,768, 16, SDL_SWSURFACE| SDL_RESIZABLE);
  SDL_WM_SetCaption("Audio Example",0);
  
  SDL_PauseAudio(0);

  bool running = true;

  SDL_Event event;
  while (running) {
    while (SDL_PollEvent(&event)) {
      /* GLOBAL KEYS / EVENTS */
      switch (event.type) {
      case SDL_KEYDOWN:
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  running = false;
	  break;
	default: break;
	}
	break;
      case SDL_MOUSEBUTTONDOWN:
	clash_kind=clash_kind_mouse;
	clash_kind_mouse++;
	if (clash_kind_mouse >4) clash_kind_mouse=4;
	break;
      case SDL_QUIT:
	running = false;
	break;
      }
      SDL_Delay(1);
    }
    SDL_Delay(1);
  }
  SDL_Quit();
  return EXIT_SUCCESS;
}
