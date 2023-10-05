// Agent.cc

#include <iostream>
#include <cstdlib>
#include "Agent.h"

using namespace std;

Agent::Agent ()
{
	this->internal_state = WorldState();
}

Agent::~Agent ()
{

}

void Agent::Initialize ()
{
	this->internal_state.worldSize = 4;
	this->internal_state.agentLocation = Location (1, 1);
	this->internal_state.agentOrientation = RIGHT;
	this->internal_state.wumpusLocation = Location (4, 4); // WUMPUS LOCATION KNOWN
	// Unknown location for gold
	// No pits used in this simulation
	this->internal_state.agentAlive = true;
	this->internal_state.agentHasArrow = true;
	this->internal_state.agentHasGold = false;
	this->internal_state.agentInCave = true;
	this->internal_state.wumpusAlive = true;
}

Action Agent::Process (Percept& percept)
{
	Action action = GRAB;

	std::cout << "\nX: " << this->internal_state.agentLocation.X
	          << "\nY: " << this->internal_state.agentLocation.Y << std::endl;

	// Agent found gold, grab it
	if (percept.Glitter)
	{
		action = GRAB;
		this->internal_state.agentHasGold = true;
	}
	// Agent has the gold and reached the entrance, leave
	else if ((this->internal_state.agentHasGold) &&
	         (this->internal_state.agentLocation == Location (1,1)))
	{
		action = CLIMB;
		this->internal_state.agentInCave = false;
	}
	// Agent has an arrow, check if there is an opportunity to kill Wumpus
	else if (this->can_kill_wumpus())
	{
		action = SHOOT;
		this->internal_state.agentHasArrow = false;
		this->internal_state.wumpusAlive = false;
	}
	// No other viable action, randomly wander
	else
	{
		action = static_cast<Action>(rand() % 3);
		if (action == GOFORWARD)
		{
			this->handle_movement();
		}
		else if (action == TURNLEFT)
		{
			this->handle_rotation (TURNLEFT);
		}
		else if (action == TURNRIGHT)
		{
			this->handle_rotation (TURNRIGHT);
		}
	}

	return action;
}

void Agent::GameOver (int score)
{

}

bool Agent::can_kill_wumpus()
{
	Orientation direction_facing = this->internal_state.agentOrientation;
	int x = this->internal_state.agentLocation.X,
	    y = this->internal_state.agentLocation.Y;

	// Short circuit situations that would most commonly invalidate
	if ((!this->internal_state.agentHasArrow) ||
	    (!this->internal_state.wumpusAlive))
	{
		return false;
	}

	// Agent's position on grid is indicative of firing position	
	// FIX: x and y were swapped, meaning the agent would fire at inappropriate times
	if (((y == 4) && (direction_facing == UP)) ||
	    ((x == 4) && (direction_facing == RIGHT)))
	{
		return true;
	}
	
	return false;
}

void Agent::handle_movement()
{

	Orientation direction_facing = this->internal_state.agentOrientation;
	int x = this->internal_state.agentLocation.X,
	    y = this->internal_state.agentLocation.Y;


	// Move temporary state positioning
	// FIX: Missing `break` statements made it were internal position didn't update
	switch (direction_facing)
	{
		case UP:
			x++;
			break;
		case DOWN:
			x--;
			break;
		case RIGHT:
			y++;
			break;
		case LEFT:
			y--;
			break;
	}

	// Determine if agent would be out of bounds with the move
	if ((x > this->internal_state.worldSize) || (x < 1) ||
		(y > this->internal_state.worldSize) || (y < 1))
	{
		return;
	}
	else
	{
		// Update internal state if valid move
		this->internal_state.agentLocation = Location (x, y);
	}
}

void Agent::handle_rotation (Action rotation)
{
	Orientation new_orientation = this->internal_state.agentOrientation;

	// Rotate agent 90 degrees to the left
	if (rotation == TURNLEFT)
	{
		switch (this->internal_state.agentOrientation)
		{
			case UP:
				new_orientation = LEFT;
				break;
			case RIGHT:
				new_orientation = UP;	
				break;
			case DOWN:
				new_orientation = RIGHT;
				break;
			case LEFT:
				new_orientation = DOWN;
				break;
		}
	}
	// Rotate agent 90 degrees to the right
	else if (rotation == TURNRIGHT)
	{
		switch (this->internal_state.agentOrientation)
		{
			case UP:
				new_orientation = RIGHT;
				break;
			case RIGHT:
				new_orientation = DOWN;
				break;
			case DOWN:
				new_orientation = LEFT;
				break;
			case LEFT:
				new_orientation = UP;
				break;
		}
	}

	// Update internal state of agent's orientation
	this->internal_state.agentOrientation = new_orientation;
}