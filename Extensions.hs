
module Extensions where

data Extension
  =  ASFExt          ASFExt         
  |  FLACExt         FLACExt        
  |  M4AExt          M4AExt         
  |  MonkeysAudioExt MonkeysAudioExt
  |  MP3Ext          MP3Ext         
  |  MusepackExt     MusepackExt    
  |  OggSpeexExt     OggSpeexExt    
  |  OggVorbisExt    OggVorbisExt   
  |  TrueAudioExt    TrueAudioExt   
  |  WavPackExt      WavPackExt     
  |  OptimFROGExt    OptimFROGExt   
  deriving Show

data ASFExt
  = ASF
  | WMA
  deriving Show

data FLACExt
  = FLAC
  deriving Show

data M4AExt
  = M4A
  | MP4
  | M4B
  | M4P
  deriving Show

data MonkeysAudioExt 
  = APE
  deriving Show

data MP3Ext          
  = MP3
  deriving Show

data MusepackExt     
  = MPC
  | MPPlus
  | MPP
  deriving Show

data OggSpeexExt     
  = SPX
  deriving Show

data OggVorbisExt    
  = OGG
  | OGA
  deriving Show

data TrueAudioExt    
  = TTA
  deriving Show

data WavPackExt      
  = WV
  deriving Show

data OptimFROGExt    
  = OFR
  deriving Show

