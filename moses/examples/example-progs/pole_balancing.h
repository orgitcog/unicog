#ifndef POLEBALANCINGINC
#define POLEBALANCINGINC

#include <moses/comboreduct/combo/simple_nn.h>

using namespace opencog;

class CartPole {
public:
    CartPole(bool randomize,bool velocity);
    virtual ~CartPole() {}
    virtual void simplifyTask();  
    virtual void nextTask();
    virtual double evalNet(combo::ann *net);
    double maxFitness;
    bool MARKOV;

    bool last_hundred;
    bool nmarkov_long;  //Flag that we are looking at the champ
    bool generalization_test;  //Flag we are testing champ's generalization

    double state[6];

    double jigglestep[1000];

protected:
    virtual void init(bool randomize);

private:

    void performAction(double output,int stepnum);
    void step(double action, double *state, double *derivs);
    void rk4(double f, double y[], double dydx[], double yout[]);
    bool outsideBounds(); 

    // C++11 constexpr constants for better compile-time optimization
    static constexpr int NUM_INPUTS = 4;
    static constexpr double MUP = 0.000002;
    static constexpr double MUC = 0.0005;
    static constexpr double GRAVITY = 9.8;
    static constexpr double MASSCART = 1.0;
    static constexpr double MASSPOLE_1 = 0.1;

    const static double LENGTH_1;		  /* actually half the pole's length */

    const static double FORCE_MAG;
    const static double TAU;		  //seconds between state updates 

    const static double one_degree;	/* 2pi/360 */
    const static double six_degrees;
    const static double twelve_degrees;
    const static double fifteen_degrees;
    const static double thirty_six_degrees;
    const static double fifty_degrees;

    double LENGTH_2;
    double MASSPOLE_2;
    double MIN_INC;
    double POLE_INC;
    double MASS_INC;

    //Queues used for Gruau's fitness which damps oscillations
    int balanced_sum;
    double cartpos_sum;
    double cartv_sum;
    double polepos_sum;
    double polev_sum;
};

#endif
