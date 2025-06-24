# 🏘️ Safehood - Neighborhood Safety DAO

A decentralized autonomous organization (DAO) for community-driven neighborhood safety initiatives. Pool resources, vote on safety proposals, and coordinate security equipment installations through blockchain governance.

## 🚀 Features

- **👥 DAO Membership**: Join the community safety initiative
- **💰 Funding Pool**: Contribute STX to the community treasury  
- **🗳️ Proposal System**: Create and vote on safety initiatives
- **📊 Incident Reporting**: Report and track neighborhood safety incidents
- **📹 Equipment Management**: Track security cameras, lighting, and patrol equipment
- **🔒 Transparent Governance**: All decisions made through community voting

## 📋 Contract Functions

### Public Functions

#### Membership
- `join-dao()` - Join the neighborhood safety DAO
- `contribute(amount)` - Contribute STX to the community treasury

#### Governance  
- `create-proposal(title, description, amount, type)` - Create funding proposals
- `vote-on-proposal(proposal-id, vote-for)` - Vote on active proposals
- `execute-proposal(proposal-id)` - Execute passed proposals

#### Safety Features
- `report-incident(location, type, description)` - Report safety incidents
- `verify-incident(incident-id)` - Verify reported incidents (owner only)

#### Equipment Management
- `add-equipment(type, location, cost)` - Add equipment to registry (owner only)
- `mark-equipment-installed(equipment-id)` - Mark equipment as installed (owner only)

### Read-Only Functions

- `get-proposal(proposal-id)` - Get proposal details
- `get-member-status(member)` - Check membership status
- `get-member-contribution(member)` - Get member's total contributions
- `get-treasury-balance()` - Get current treasury balance
- `get-incident(incident-id)` - Get incident details
- `get-equipment(equipment-id)` - Get equipment details
- `get-vote(proposal-id, voter)` - Get specific vote
- `get-proposal-count()` - Get total proposals
- `get-incident-count()` - Get total incidents
- `get-equipment-count()` - Get total equipment

## 🛠️ Usage Examples

### Join the DAO and Contribute
```clarity
(contract-call? .safehood join-dao)
(contract-call? .safehood contribute u1000000) ;; 1 STX
```

### Create a Security Camera Proposal
```clarity
(contract-call? .safehood create-proposal 
    "Security Camera Installation" 
    "Install 4K security camera at Main St intersection"
    u5000000  ;; 5 STX
    "camera")
```

### Vote on Proposals
```clarity
(contract-call? .safehood vote-on-proposal u1 true) ;; Vote yes on proposal 1
```

### Report Safety Incidents
```clarity
(contract-call? .safehood report-incident 
    "Main St & Oak Ave" 
    "vandalism" 
    "Graffiti on community center wall")
```

## 🏗️ Deployment

Deploy using Clarinet:

```bash
clarinet deploy
```

## 🔐 Security Features

- Member-only proposal creation and voting
- Owner-only incident verification and equipment management
- Voting period enforcement (144 blocks ≈ 24 hours)
- Treasury balance validation
- Double-voting prevention

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is open source and available under the MIT License.

---

*Building safer neighborhoods through decentralized governance* 🏠✨

