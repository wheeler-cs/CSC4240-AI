// Agent.h

#ifndef AGENT_H
#define AGENT_H

#include "Action.h"
#include "Percept.h"
#include "WorldState.h"

class Agent
{
public:
	Agent ();
	~Agent ();
	void Initialize ();
	Action Process (Percept& percept);
	void GameOver (int score);

	bool can_kill_wumpus();
	void handle_movement();
	void handle_rotation (Action);

private:
	WorldState internal_state;
};

#endif // AGENT_H
