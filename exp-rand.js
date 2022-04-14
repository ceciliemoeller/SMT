// var stimuliExpRandomizer = {},
//     stimuliExpRandomizerRandomNb = Math.floor(Math.random() * 10);

// if (stimuliExpRandomizerRandomNb < 3){
//     stimuliExpRandomizerRandomNb = 0;
// }
// else if (stimuliExpRandomizerRandomNb < 6){
//     stimuliExpRandomizerRandomNb = 1;
// }
// else {
//     stimuliExpRandomizerRandomNb = 2;
// }

var stimuliExpRandomizer = {},
    stimuliExpRandomizerRandomNb = Math.floor(Math.random() * 100);

if (stimuliExpRandomizerRandomNb < 101){
    stimuliExpRandomizerRandomNb = 0;
}
// ####### the following six lines + ll 40-41 + ll 64-107 are used when several experiments are run in the same study ##############   
// else if (stimuliExpRandomizerRandomNb < 60){
//     stimuliExpRandomizerRandomNb = 1;
// }
// else {
//     stimuliExpRandomizerRandomNb = 2;
// }

/**
 * This function takes a type parameter, deciding
 * if we are asking for preload or exp data.
 * 
 * When requesting preload data, we take a random
 * array of stimulis. This result is stored so that
 * we can return the same array when asking for exp data.
 */
stimuliExpRandomizer.randomizeStimuli = function(type) {
    var randomNb = stimuliExpRandomizerRandomNb,
        sound_check_stim = [
            "sounds/pitch/poly_pitch_marimba_loudness.mp3",
            // "sounds/tempo/poly_tempo_loudness_check.mp3",
            // "sounds/ratio/poly_ratio_loudness_check.mp3",
        ],
        dataArray = [
            [                                        
                "sounds/pitch/p01_2-C3-07_3-C5-07.mp3",
                "sounds/pitch/p02_2-C3-04_3-C5-10.mp3",
                "sounds/pitch/p03_2-C3-01_3-C5-13.mp3",
                "sounds/pitch/p04_2-C5-07_3-C3-07.mp3",
                "sounds/pitch/p05_2-C5-04_3-C3-10.mp3",
                "sounds/pitch/p06_2-C5-01_3-C3-13.mp3",
                "sounds/pitch/p07_3-C3-07_4-C5-07.mp3",
                "sounds/pitch/p08_3-C3-10_4-C5-04.mp3",              
                "sounds/pitch/p09_3-C3-13_4-C5-01.mp3",               
                "sounds/pitch/p10_3-C5-07_4-C3-07.mp3",              
                "sounds/pitch/p11_3-C5-10_4-C3-04.mp3",            
                "sounds/pitch/p12_3-C5-13_4-C3-01.mp3",
                "sounds/pitch/p13_2-C4-07_3-C4-07.mp3",
                "sounds/pitch/p14_2-C4-04_3-C4-10.mp3",
                "sounds/pitch/p15_2-C4-01_3-C4-13.mp3",
                "sounds/pitch/p16_3-C4-07_4-C4-07.mp3",              
                "sounds/pitch/p17_3-C4-10_4-C4-04.mp3",            
                "sounds/pitch/p18_3-C4-13_4-C4-01.mp3"         
            ],
            // [
            //     "sounds/tempo/t01_2_3_040_060.mp3",
            //     "sounds/tempo/t02_2_3_060_090.mp3",
            //     "sounds/tempo/t03_2_3_090_135.mp3",
            //     "sounds/tempo/t04_2_3_120_180.mp3",
            //     "sounds/tempo/t05_2_3_135_203.mp3",
            //     "sounds/tempo/t06_2_3_169_253.mp3",
            //     "sounds/tempo/t07_2_3_203_304.mp3",
            //     "sounds/tempo/t08_2_3_304_456.mp3",
            //     "sounds/tempo/t09_3_4_038_051.mp3",
            //     "sounds/tempo/t10_3_4_051_068.mp3",
            //     "sounds/tempo/t11_3_4_068_090.mp3",
            //     "sounds/tempo/t12_3_4_090_120.mp3",
            //     "sounds/tempo/t13_3_4_120_160.mp3",               
            //     "sounds/tempo/t14_3_4_160_213.mp3",
            //     "sounds/tempo/t15_3_4_187_249.mp3",
            //     "sounds/tempo/t16_3_4_213_284.mp3",
            //     "sounds/tempo/t17_3_4_284_379.mp3",
            //     "sounds/tempo/t18_3_4_379_506.mp3"
            // ],
            // [
            //     "sounds/ratio/r01_2_3_125ms.mp3",
            //     "sounds/ratio/r02_2_3_167ms.mp3",
            //     "sounds/ratio/r03_2_3_250ms.mp3",
            //     "sounds/ratio/r04_2_3_333ms.mp3",
            //     "sounds/ratio/r05_2_5_125ms.mp3",
            //     "sounds/ratio/r06_2_5_167ms.mp3",
            //     "sounds/ratio/r07_2_5_250ms.mp3",
            //     "sounds/ratio/r08_2_5_333ms.mp3",
            //     "sounds/ratio/r09_3_4_125ms.mp3",
            //     "sounds/ratio/r10_3_4_167ms.mp3",
            //     "sounds/ratio/r11_3_5_063ms.mp3",
            //     "sounds/ratio/r12_3_5_084ms.mp3",
            //     "sounds/ratio/r13_3_5_125ms.mp3",
            //     "sounds/ratio/r14_3_5_167ms.mp3",
            //     "sounds/ratio/r15_4_5_063ms.mp3",
            //     "sounds/ratio/r16_4_5_084ms.mp3",
            //     "sounds/ratio/r17_4_5_125ms.mp3",
            //     "sounds/ratio/r18_4_5_167ms.mp3",
            //     "sounds/ratio/r19_5_6_063ms.mp3",                         
            //     "sounds/ratio/r20_5_6_084ms.mp3",             
            //     "sounds/ratio/r21_5_6_125ms.mp3",              
            //     "sounds/ratio/r22_5_6_167ms.mp3"                   
            // ],
        ];

    switch (type) {
        case 'preload':
            console.log('number is ' + randomNb)
            return dataArray[randomNb]
            break;
        case 'exp':
            console.log('number is ' + randomNb)
            return [sound_check_stim[randomNb],stimuliExpRandomizer.createExpArray(dataArray[randomNb])]
            break;
        default:
            break;
    }
}

stimuliExpRandomizer.createExpArray = function(myArray) {
    var myObj = {},
        myNewArray = [];
    myArray.forEach(element => {
        myObj = { 'stimulus': element };
        myNewArray.push(myObj)
    });
    console.dir(myNewArray)
    return myNewArray;
}