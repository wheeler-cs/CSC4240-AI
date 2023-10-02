// Agent.cc

#include <iostream>
#include <cstdlib>
#include "Agent.h"

using namespace std;

Agent::Agent ()
{
	srand (time (NULL));
}

Agent::~Agent ()
{

}

void Agent::Initialize ()
{

}

Action Agent::Process (Percept& percept)
{
	Action action;

	if (percept.Glitter)
	{
		action = GRAB;
	}
	else if ((this->internal_state.agentHasGold) &&
	         (this->internal_state.agentLocation == Location (1,1)))
	{
		action = CLIMB;
	}
	else if (this->internal_state.agentHasArrow)
	{
		if ((this->internal_state.agentLocation.X == 4) &&
		    (this->internal_state.agentOrientation == UP))
		{
			action = SHOOT;
		}
		else if ((this->internal_state.agentLocation.Y == 4) &&
		         (this->internal_state.agentOrientation == RIGHT))
		{
			action = SHOOT;
		}
	}
	else
	{
		action = (Action)(rand() % 3);
	}

	return action;
}

void Agent::GameOver (int score)
{

}

